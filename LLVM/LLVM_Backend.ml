open Base
open Imp
open ImpHelpers
open Imp_to_LLVM
open Llvm
open Value

module GV = Llvm_executionengine.GenericValue
module LLE = Llvm_executionengine.ExecutionEngine

let _ = Llvm_executionengine.initialize_native_target()
let execution_engine = LLE.create Imp_to_LLVM.global_module

(** Multithreaded CPU Work Queue **)
external create_work_queue : int -> Int64.t = "ocaml_create_work_queue"
external destroy_work_queue : Int64.t -> unit = "ocaml_destroy_work_queue"
external do_work : Int64.t -> LLE.t -> llvalue -> GV.t list list -> unit =
    "ocaml_do_work"
(* TODO: For now, hard code the number of threads *)
let work_queue = create_work_queue 8

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
  Llvm_scalar_opts.add_type_based_alias_analysis the_fpm;
  Llvm_scalar_opts.add_ind_var_simplification the_fpm;
  Llvm_scalar_opts.add_dead_store_elimination the_fpm;
  Llvm_scalar_opts.add_memcpy_opt the_fpm;
  Llvm_scalar_opts.add_gvn the_fpm;
  Llvm_scalar_opts.add_correlated_value_propagation the_fpm;

  Llvm_scalar_opts.add_licm the_fpm;
  Llvm_scalar_opts.add_loop_unswitch the_fpm;
  Llvm_scalar_opts.add_loop_unroll the_fpm;
  Llvm_scalar_opts.add_loop_unroll the_fpm;
  Llvm_scalar_opts.add_loop_rotation the_fpm;
  Llvm_scalar_opts.add_loop_idiom the_fpm;
  Llvm_scalar_opts.add_type_based_alias_analysis the_fpm;
  Llvm_scalar_opts.add_basic_alias_analysis the_fpm;
  let _ : bool = PassManager.run_function llvmFn the_fpm in
  let _ : bool = PassManager.finalize the_fpm in
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
let allocate_output_arrays impFn args : Ptr.t Value.t list =
  let inputShapes : Shape.t list = List.map Value.get_shape args in
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
let allocate_output_generic_values impFn args : GV.t list =
  let inputShapes : Shape.t list = List.map Value.get_shape args in
  (* compute output shapes from input shapes *)
  let outputShapes = ShapeEval.get_call_output_shapes impFn inputShapes in
  List.map2 allocate_output_gv (Imp.output_types impFn) outputShapes



let free_scalar_output impT (gv:GV.t) : unit =
  if ImpType.is_scalar impT then HostMemspace.free (GV.as_int64 gv)

let free_scalar_outputs impTypes gvs =
  List.iter2 free_scalar_output impTypes gvs

let split_argument num_items arg =
	match arg with
	| Scalar n ->
	  List.fill (Value_to_GenericValue.to_llvm (Scalar n)) (List.til num_items)
	| Array {data; array_type; elt_type; array_shape; array_strides} ->
	  (* TODO: for now, only support splitting rank 1 arrays. *)
	  assert (Shape.rank array_shape == 1);
	  let make_item
        {data; array_type; elt_type; array_shape; array_strides}
        els
        offset =
      let newshape = Shape.of_list [els] in
	    let newaddr = Ptr.get_slice data ((Type.sizeof elt_type) * offset) in
	    Value_to_GenericValue.to_llvm
	        (Array {data=newaddr; array_type; elt_type;
	                array_shape=newshape; array_strides})
	  in
    (* TODO: does a 0-length slice work? *)
    let len = Shape.get array_shape 0 in
    let els_per_item = safe_div len num_items in
    let last_num = len - (els_per_item * (num_items - 1)) in
    let nums_els = (List.fill els_per_item (List.til (num_items - 1))) @
                   [last_num] in
    let mul x y = x * y in
	  let os = List.map (mul els_per_item) (List.til num_items) in
	  List.map2
	      (make_item {data; array_type; elt_type; array_shape; array_strides})
        nums_els
	      os
	| _ -> failwith "Unsupported argument type for splitting."

(* TODO: 1. We can easily share the LLVM descriptors amongst the chunks.
            To make things easier to get running, I'll split the Values rather
            that the LLVM GVs, and thus duplicate these structs.
*)
let build_work_items args num_items =
  let list_of_split = List.map (split_argument num_items) args in
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
        Printf.printf
          "[LLVM_Backend] Got cached code for %s\n%!"
          (FnId.to_str fnId)
        ;
        llvmFn
      end
    | None ->
      begin
        let llvmFn : Llvm.llvalue = Imp_to_LLVM.compile_fn impFn in
        optimize_module Imp_to_LLVM.global_module llvmFn;
        print_endline  "[LLVM_Backend.call_imp_fn] Generated LLVM function";
        Llvm.dump_value llvmFn;
        Llvm_analysis.assert_valid_function llvmFn;
        Hashtbl.add cache fnId llvmFn;
        llvmFn
      end
end

let call_imp_fn (impFn:Imp.fn) (args:Ptr.t Value.t list) : Ptr.t Value.t list =
  let llvmFn = CompiledFunctionCache.compile impFn in
  let llvmInputs : GV.t list = List.map Value_to_GenericValue.to_llvm args in
  let llvmOutputs : GV.t list = allocate_output_generic_values impFn args in
  Printf.printf "[LLVM_Backend.call_imp_fn] Running function\n%!";
  let impInputTypes = Imp.input_types impFn in
  let impOutputTypes = Imp.output_types impFn in
  Printf.printf "  -- input params: %s\n%!"
    (Value.list_to_str
      (List.map2 (GenericValue_to_Value.of_generic_value ~boxed_scalars:false)
                  llvmInputs impInputTypes));
  let params : GV.t array = Array.of_list (llvmInputs @ llvmOutputs) in
  let _ = LLE.run_function llvmFn params execution_engine in
  Printf.printf " :: function completed\n%!";
  let outputs =
    List.map2 GenericValue_to_Value.of_generic_value llvmOutputs impOutputTypes
  in
  free_scalar_outputs impOutputTypes llvmOutputs;
  Printf.printf
    "[LLVM_Backend.call_imp_fn] Got function results: %s\n%!"
    (Value.list_to_str outputs)
  ;
  outputs

let call (fn:SSA.fn) args =
  let inputTypes = List.map ImpType.type_of_value args in
  let impFn : Imp.fn = SSA_to_Imp.translate_fn fn inputTypes in
  call_imp_fn impFn args

let map ~axes ~fn ~fixed args =
  let inputTypes = List.map ImpType.type_of_value args in
  let impFn : Imp.fn =
    SSA_to_Imp.translate_fn fn inputTypes in
  let llvmFn = CompiledFunctionCache.compile impFn in
  let outputs : Ptr.t Value.t list  = allocate_output_arrays impFn args in
  let work_items = build_work_items (args @ outputs) 8 in
  do_work work_queue execution_engine llvmFn work_items;
  Printf.printf
    "[LLVM_Backend.call_imp_fn] Got function results: %s\n%!"
    (Value.list_to_str outputs)
  ;
  outputs

let reduce ~axes ~fn ~fixed ?init args = assert false

let scan ~axes ~fn ~fixed ?init args = assert false

let allpairs ~axes ~fn ~fixed x y = assert false

let array_op p args = assert false
