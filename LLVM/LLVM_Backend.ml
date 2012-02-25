(* pp: -parser o pa_macro.cmo *)

open Base
open Imp
open ImpHelpers
open Imp_to_LLVM
open Llvm
open Value

module GV = Llvm_executionengine.GenericValue
module LLE = Llvm_executionengine.ExecutionEngine

let _ = Llvm_executionengine.initialize_native_target()

(* set opt-level to 3 *)
let execution_engine = LLE.create_jit Imp_to_LLVM.global_module 3

(** Multithreaded CPU Work Queue **)
external create_work_queue : int -> Int64.t = "ocaml_create_work_queue"
external destroy_work_queue : Int64.t -> unit = "ocaml_destroy_work_queue"
external do_work : Int64.t -> LLE.t -> llvalue -> GV.t list list -> unit =
    "ocaml_do_work"
(* TODO: For now, hard code the number of threads *)
let num_cores = 8
let work_queue = create_work_queue num_cores

let optimize_module llvmModule llvmFn : unit =
  let the_fpm = PassManager.create_function llvmModule in
  (* Set up the optimizer pipeline.  Start with registering info about how the
   * target lays out data structures. *)
  Llvm_target.TargetData.add (LLE.target_data execution_engine) the_fpm;

  (* Promote allocas to registers. *)
  Llvm_scalar_opts.add_memory_to_register_promotion the_fpm;
  Llvm_scalar_opts.add_sccp the_fpm;
  Llvm_scalar_opts.add_aggressive_dce the_fpm;
  Llvm_scalar_opts.add_instruction_combination the_fpm;
  Llvm_scalar_opts.add_cfg_simplification the_fpm;
  Llvm_scalar_opts.add_gvn the_fpm;
  Llvm_scalar_opts.add_licm the_fpm;
  Llvm_scalar_opts.add_loop_unroll the_fpm;

  ignore (PassManager.run_function llvmFn the_fpm);
  ignore (PassManager.finalize the_fpm);
  PassManager.dispose the_fpm

let memspace_id = HostMemspace.id

let strides_from_shape shape eltSize =
  let rank = Shape.rank shape in
  let strides = Array.create rank eltSize in
  for i = rank - 2 downto 0 do
    strides.(i) <- strides.(i+1) * (Shape.get shape (i+1))
  done;
  strides

let allocate_array eltT shape : Ptr.t Value.t =
  let eltSize : int = Type.sizeof eltT in
  let nelts = Shape.nelts shape in
  let rank = Shape.rank shape in
  Array {
    data = Mem.alloc HostMemspace.id (nelts * eltSize);
    array_type = Type.ArrayT(eltT, rank);
    elt_type = eltT;
    array_shape = shape;
    array_strides = strides_from_shape shape (Type.sizeof eltT);
  }

(* given a function and a list of its arguments, *)
(* allocate space for the outputs and return them as Parakeet Values. *)
(* NOTE: This is only valid for arrays, scalars will crash *)
let allocate_output_arrays impFn inputShapes : Ptr.t Value.t list =
  (* compute output shapes from input shapes *)
  let outputShapes = ShapeEval.get_call_output_shapes impFn inputShapes in
  let outTypes = Imp.output_types impFn in
  assert (List.for_all ImpType.is_array outTypes);
  let eltTypes = List.map ImpType.elt_type outTypes in
  List.map2 allocate_array eltTypes outputShapes

let allocate_output_gv impT (shape:Shape.t) : GV.t  =
  match impT with
  | ImpType.ScalarT eltT ->
    GV.of_int64 LLVM_Types.int64_t (HostMemspace.malloc (Type.sizeof eltT))
  | ImpType.ArrayT (eltT, _) ->
    Value_to_GenericValue.to_llvm (allocate_array eltT shape)

(* given a function and a list of its arguments, *)
(* allocate space for the outputs and return them as LLVM's GenericValues  *)
let allocate_output_generic_values impFn inputShapes : GV.t list =
  IFDEF DEBUG THEN
    Printf.printf
      "[LLVM_Backend] Inferring output shapes of %s with input shapes [%s]\n"
      (FnId.to_str impFn.Imp.id)
      (String.concat ", " (List.map Shape.to_str inputShapes));
  ENDIF;
  (* compute output shapes from input shapes *)
  let outputShapes = ShapeEval.get_call_output_shapes impFn inputShapes in
  List.map2 allocate_output_gv (Imp.output_types impFn) outputShapes

let free_scalar_output impT (gv:GV.t) : unit =
  if ImpType.is_scalar impT then HostMemspace.free (GV.as_int64 gv)

let free_scalar_outputs impTypes gvs =
  List.iter2 free_scalar_output impTypes gvs

let split_argument axes num_items arg =
	match arg with
	| Scalar n ->
	  List.fill (Value_to_GenericValue.to_llvm (Scalar n)) (List.til num_items)
	| Array {data; array_type; elt_type; array_shape; array_strides} ->
    (* TODO: for now, we just split the longest axis. Come up with something *)
    (*       good later. *)
    (* TODO: does a 0-length slice work? *)
    let longest_axis = ref 0 in
    let len = ref 0 in
    Array.iteri (fun idx dim ->
      if dim > !len then (
        longest_axis := idx;
        len := dim
      ))
      (Shape.to_array array_shape)
    ;
    let els_per_item = safe_div !len num_items in
    let mul x y = x * y in
    let starts = List.map (mul els_per_item) (List.til num_items) in
    let stops =
      (List.map (mul els_per_item) (List.range 1 (num_items - 1))) @ [!len]
    in
    let make_slice start stop = Value.Slice(arg, !longest_axis, start, stop) in
    let slices = List.map2 make_slice starts stops in
    List.map Value_to_GenericValue.to_llvm slices
	| _ -> failwith "Unsupported argument type for splitting."

(* TODO: 1. We can easily share the LLVM descriptors amongst the chunks.
            To make things easier to get running, I'll split the Values rather
            that the LLVM GVs, and thus duplicate these structs.
*)
let build_work_items axes num_items args =
  let list_of_split = List.map (split_argument axes num_items) args in
  let get_i i l = List.nth l i in
  let strip_is i l = [List.map (get_i i) l] in
  let rec flip_l cur i stop l =
    if i == stop then cur
    else let next = cur @ (strip_is i l) in
    flip_l next (i+1) stop l
  in
  let first = strip_is 0 list_of_split in
  flip_l first 1 (List.length (List.nth list_of_split 0)) list_of_split

module CompiledFunctionCache = struct
  let cache : (FnId.t, Llvm.llvalue) Hashtbl.t = Hashtbl.create 127
  let compile impFn =
    let fnId = impFn.Imp.id in
    match Hashtbl.find_option cache fnId with
    | Some llvmFn ->
      begin
        IFDEF DEBUG THEN
          Printf.printf
            "[LLVM_Backend] Got cached code for %s\n%!"
            (FnId.to_str fnId)
          ;
        ENDIF;
        llvmFn
      end
    | None ->
      begin
        let llvmFn : Llvm.llvalue = Imp_to_LLVM.compile_fn impFn in
        optimize_module Imp_to_LLVM.global_module llvmFn;
        IFDEF DEBUG THEN
          print_endline  "[LLVM_Backend.call_imp_fn] Generated LLVM function";
          Llvm.dump_value llvmFn;
          Llvm_analysis.assert_valid_function llvmFn;
        ENDIF;
        Hashtbl.add cache fnId llvmFn;
        llvmFn
      end
end

let call_imp_fn (impFn:Imp.fn) (args:Ptr.t Value.t list) : Ptr.t Value.t list =
  IFDEF DEBUG THEN
    Printf.printf "[LLVM_Backend.call_imp_fn] Calling %s with inputs %s\n"
      (FnId.to_str impFn.Imp.id)
      (String.concat ", " (List.map Value.to_str args));
  ENDIF;
  let llvmFn = CompiledFunctionCache.compile impFn in
  let llvmInputs : GV.t list = List.map Value_to_GenericValue.to_llvm args in
  let argShapes = List.map Value.shape_of args in
  let llvmOutputs : GV.t list =
    allocate_output_generic_values impFn argShapes
  in
  IFDEF DEBUG THEN
    Printf.printf "[LLVM_Backend.call_imp_fn] Running function\n%!";
  ENDIF;
  let impInputTypes = Imp.input_types impFn in
  let impOutputTypes = Imp.output_types impFn in
  IFDEF DEBUG THEN
    let convert_gv =
      GenericValue_to_Value.of_generic_value ~boxed_scalars:false
    in
    let vals = List.map2 convert_gv  llvmInputs impInputTypes in
    Printf.printf "  -- input params: %s\n%!" (Value.list_to_str vals);
  ENDIF;
  let params : GV.t array = Array.of_list (llvmInputs @ llvmOutputs) in
  let _ = LLE.run_function llvmFn params execution_engine in
  IFDEF DEBUG THEN
    Printf.printf " :: function completed\n%!";
  ENDIF;
  let outputs =
    List.map2 GenericValue_to_Value.of_generic_value llvmOutputs impOutputTypes
  in
  free_scalar_outputs impOutputTypes llvmOutputs;
  IFDEF DEBUG THEN
    Printf.printf
      "[LLVM_Backend.call_imp_fn] Got function results: %s\n%!"
      (Value.list_to_str outputs)
    ;
  ENDIF;
  outputs

let call (fn:TypedSSA.fn) args =
  let inputTypes = List.map ImpType.type_of_value args in
  let impFn : Imp.fn = SSA_to_Imp.translate_fn fn inputTypes in
  call_imp_fn impFn args

let adverb (info:(TypedSSA.fn, Ptr.t Value.t list, int list) Adverb.info) args =
  (* make another Adverb.info object to call into AdverbHelpers.mk_adverb_fn *)
  let ssaInfo :
    (FnId.t, Type.t list, TypedSSA.value_nodes) Adverb.info =
    Adverb.apply_to_fields
      ~fn:TypedSSA.fn_id
      ~args:Value.type_of_list
      ~axes:(List.map TypedSSA.int32)
      info
  in
  let adverbFn = AdverbHelpers.mk_adverb_fn ssaInfo (Value.type_of_list args) in
  assert (Adverb.init info = None);
  let fixed = Adverb.fixed_args info in
  let impTypes = List.map ImpType.type_of_value (fixed @ args) in
  let impFn : Imp.fn = SSA_to_Imp.translate_fn adverbFn impTypes in
  let llvmFn = CompiledFunctionCache.compile impFn in
  let inputShapes : Shape.t list = List.map Value.get_shape (fixed @ args) in
  let outputs : Ptr.t Value.t list =
    allocate_output_arrays impFn inputShapes
  in
  let axes = Adverb.axes info in
  let work_items = build_work_items axes num_cores (args @ outputs) in
  do_work work_queue execution_engine llvmFn work_items;
  (* TODO: What happens to reduced things? We still need to recombine them!*)
  IFDEF DEBUG THEN
    Printf.printf
      "[LLVM_Backend.call_imp_fn] Got function results: %s\n%!"
      (Value.list_to_str outputs)
    ;
  ENDIF;
  outputs
