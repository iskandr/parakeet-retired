(* pp: -parser o pa_macro.cmo *)

open Adverb
open Base
open Imp
open ImpHelpers
open Imp_to_LLVM
open Llvm
open MachineModel
open Value

let memspace_id = HostMemspace.id

module GV = Llvm_executionengine.GenericValue
module LLE = Llvm_executionengine.ExecutionEngine

let _ = Llvm_executionengine.initialize_native_target()
(* set native opt-level to 3 *)
let execution_engine = LLE.create_jit Imp_to_LLVM.llvm_module 3

(** Multithreaded CPU Work Queue **)
let use_multithreading = ref true
let set_multithreading b = use_multithreading := b
external create_work_queue : int -> Int64.t = "ocaml_create_work_queue"
external destroy_work_queue : Int64.t -> unit = "ocaml_destroy_work_queue"
external do_work : Int64.t -> LLE.t -> llvalue -> GV.t list list -> unit =
    "ocaml_do_work"

let num_threads = MachineModel.num_hw_threads
let work_queue = create_work_queue num_threads

(* For now, we are assuming only 1 CPU *)
let vector_bitwidth = MachineModel.machine_model.cpus.(0).vector_bitwidth

let optimize_module llvmModule llvmFn : unit =
  let pm = PassManager.create_function llvmModule in
  (* Set up the optimizer pipeline.  Start with registering info about how the
   * target lays out data structures. *)
  Llvm_target.TargetData.add (LLE.target_data execution_engine) pm;

  (* Promote allocas to registers. *)
  Llvm_scalar_opts.add_verifier pm;
  (* memory to register *)

  Llvm_scalar_opts.add_memory_to_register_promotion pm;
  Llvm_scalar_opts.add_scalar_repl_aggregation_ssa pm;
  Llvm_scalar_opts.add_scalar_repl_aggregation pm;

  (* basic optimizations *)
  Llvm_scalar_opts.add_cfg_simplification pm;
  Llvm_scalar_opts.add_instruction_combination pm;
  Llvm_scalar_opts.add_sccp pm;
  Llvm_scalar_opts.add_gvn pm;
  Llvm_scalar_opts.add_correlated_value_propagation pm;
  Llvm_scalar_opts.add_basic_alias_analysis pm;

  (* loop optimizations *)
  Llvm_scalar_opts.add_licm pm;
  Llvm_scalar_opts.add_loop_unroll pm;
  Llvm_scalar_opts.add_loop_deletion pm;
  Llvm_scalar_opts.add_loop_unswitch pm;
  Llvm_scalar_opts.add_ind_var_simplification pm;

  (* dead code elimination *)
  Llvm_scalar_opts.add_dead_store_elimination pm;
  Llvm_scalar_opts.add_aggressive_dce pm;

  ignore (PassManager.run_function llvmFn pm);
  ignore (PassManager.finalize pm);
  PassManager.dispose pm

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
    GV.of_int64 LlvmType.int64_t (HostMemspace.malloc (Type.sizeof eltT))
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

let get_arg_alignment arg =
  match arg with
  | Scalar _ -> 1
  | Array {elt_type} ->
    (* TODO: I'm just assuming this is the correct alignment formula... *)
    let byte_alignment = vector_bitwidth / 8 in
    let el_size = Type.sizeof elt_type in
    let q = byte_alignment / el_size in
    if q * el_size = byte_alignment then q
    else q * byte_alignment

(* TODO: All this scheduling logic is really messy. As in terrible. *)
let do_parallel info =
  if not !use_multithreading then false
  else
    let arg = List.hd info.array_args in
    match arg with
    | Scalar _ -> true
    | Array {array_shape} ->
      let min_elts = (get_arg_alignment arg) * num_threads in
      (Shape.get array_shape 0) >= min_elts

let split_argument axes num_items alignment arg =
  match arg with
  | Scalar n ->
    List.fill (Value_to_GenericValue.to_llvm (Scalar n)) (List.til num_items)
  | Array {data; array_type; elt_type; array_shape; array_strides} ->
    (* TODO: for now, we just split the first axis. Come up with something *)
    (*       good later. *)
    let longest_axis = ref 0 in
    let len = ref 0 in
    Array.iteri (fun idx dim ->
      if dim > !len then (
        longest_axis := idx;
        len := dim
      ))
      (Shape.to_array array_shape)
    ;
    longest_axis := 0;
    len := Shape.get array_shape 0;
    (* TODO: For now, an easy division of the elements that works. Probably *)
    (*       want to do something slightly more sophisticated so as to evenly *)
    (*       divide the work amongst the threads. *)
    let els_per_item = (!len / num_items / alignment) * alignment in
    let mul x y = x * y in
    let starts, stops =
      if els_per_item > 0 then
        let starts = List.map (mul els_per_item) (List.til num_items) in
        let stops =
          (List.map (mul els_per_item) (List.range 1 (num_items - 1))) @ [!len]
        in
        starts, stops
      else
        let starts = [0] @ (List.fill !len (List.til (num_items - 1))) in
        let stops = List.fill !len (List.til num_items) in
        starts, stops
    in
    let make_slice start stop = Value.Slice(arg, !longest_axis, start, stop) in
    let slices = List.map2 make_slice starts stops in
    List.map Value_to_GenericValue.to_llvm slices
  | _ -> failwith "Unsupported argument type for splitting."

let build_work_items axes num_items args =
  let alignments = List.map get_arg_alignment args in
  let alignment =
    List.fold_left (fun x y -> if x > y then x else y) 0 alignments
  in
  let list_of_split = List.map (split_argument axes num_items alignment) args in
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
         IFDEF DEBUG THEN
          print_endline "[LLVM_Backend.call_imp_fn] Generated LLVM function";
          Llvm.dump_value llvmFn;
        ENDIF;
        IFDEF DEBUG THEN
          Printf.printf "Validating generated function...\n%!";
          Llvm_analysis.assert_valid_function llvmFn;
        ENDIF;
        optimize_module Imp_to_LLVM.llvm_module llvmFn;
        IFDEF DEBUG THEN
          print_endline  "[LLVM_Backend.call_imp_fn] Optimized LLVM function";
          Llvm.dump_value llvmFn;
        ENDIF;
        Hashtbl.add cache fnId llvmFn;
        llvmFn
      end
end

let call_imp_fn (impFn:Imp.fn) (args:Ptr.t Value.t list) : Ptr.t Value.t list =
  IFDEF DEBUG THEN
    Printf.printf "[LLVM_Backend.call_imp_fn] Calling %s with inputs %s\n%!"
      (FnId.to_str impFn.Imp.id)
      (String.concat ", " (List.map Value.to_str args));
  ENDIF;
  let llvmFn = CompiledFunctionCache.compile impFn in
  let llvmInputs : GV.t list = List.map Value_to_GenericValue.to_llvm args in
  IFDEF DEBUG THEN
    Printf.printf "input_types: %s\n%!"
      (ImpType.type_list_to_str (Imp.input_types impFn));
    let convert_gv : GV.t -> ImpType.t -> Ptr.t Value.t  =
      GenericValue_to_Value.of_generic_value ~boxed_scalars:false
    in
    let vals = List.map2 convert_gv llvmInputs (Imp.input_types impFn) in
    Printf.printf "[LLVM_Backend.call_imp_fn] GenericValue inputs: %s\n%!"
      (Value.list_to_str vals)
    ;
  ENDIF;
  let argShapes = List.map Value.shape_of args in
  let llvmOutputs : GV.t list =
    allocate_output_generic_values impFn argShapes
  in
  let impOutputTypes = Imp.output_types impFn in
  let params : GV.t array = Array.of_list (llvmInputs @ llvmOutputs) in
  IFDEF DEBUG THEN
    Printf.printf "[LLVM_Backend.call_imp_fn] Running function\n%!";
  ENDIF;
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
      (Value.list_to_str ~array_to_str:Value.generic_array_to_str outputs)
    ;
  ENDIF;
  outputs

let call (fn:TypedSSA.fn) (args:Ptr.t Value.t list) =
  let inputTypes = List.map ImpType.type_of_value args in
  Printf.printf 
    "[LLVM_Backend.call] Creating imp function with arg types %s\n%!" 
    (String.concat ", " (List.map ImpType.to_str inputTypes))
  ; 
  let impFn : Imp.fn = SSA_to_Imp.translate_fn fn inputTypes in
  call_imp_fn impFn args

let exec_map impFn inputShapes axes array_args llvmFn =
  let outputs : Ptr.t Value.t list =
    allocate_output_arrays impFn inputShapes
  in
  (* TODO: looks like we're ignoring the closure values! *)
  let work_items =
    build_work_items axes num_threads (array_args @ outputs)
  in
  do_work work_queue execution_engine llvmFn work_items;
  outputs

let exec_reduce impFn inputShapes axes array_args llvmFn =
  (* Have to allocate num_threads * output sizes to hold the intermediates *)
  let outputShapes = ShapeEval.get_call_output_shapes impFn inputShapes in
  let outTypes = Imp.output_types impFn in
  let eltTypes = List.map ImpType.elt_type outTypes in
  let get_interm_shape shape = function
    | ImpType.ScalarT _ ->
      let interm_shape = Shape.create 1 in
      Shape.set interm_shape 0 num_threads;
      interm_shape
    | ImpType.ArrayT (_, _) ->
      let t_shape = Shape.create 1 in
      Shape.set t_shape 0 num_threads;
      Shape.append t_shape shape
    | _ -> failwith "Unexpected output type from reduction"
  in
  let intermShapes = List.map2 get_interm_shape outputShapes outTypes in
  let interms = List.map2 allocate_array eltTypes intermShapes in
  let llvmInterms = List.map Value_to_GenericValue.to_llvm interms in
  let rec slice_interms cur i =
    if i == num_threads then
      match cur with
      | [[]] -> cur
      | hd :: rest -> rest
    else
      let get_slice arg ty shape : GV.t =
        (* Have to handle the case of slicing out scalars specially *)
        if Shape.rank shape == 1 then (
          let data = match Value.extract arg with
            | Some d -> d
            | None -> failwith "Array expected in reduce intermediate slicing"
          in
          let ptr = HostMemspace.get_ptr_to_index data.Ptr.addr ty i in
          GV.of_int64 LlvmType.int64_t ptr)
        else
          let val_slice = Value.Slice(arg, 0, i, i + 1) in
          Value_to_GenericValue.to_llvm val_slice
      in
      let slices =
        List.map3 get_slice interms eltTypes intermShapes
      in
      slice_interms (cur @ [slices]) (i + 1)
  in
  let interm_slices = slice_interms [[]] 0 in
  let input_items = build_work_items axes num_threads array_args in
  let work_items = List.map2 (fun a b -> a @ b) input_items interm_slices in
  do_work work_queue execution_engine llvmFn work_items;

  (* Change the shape of the intermediate so that the sequential function *)
  (* can use it as its input. *)
  let seqInputs =
    let num_axes = List.length axes in
    if num_axes > 1 then
      let get_dummy_shape shape =
        let t_array = Array.make (num_axes - 1) 1 in
        Shape.append (Shape.of_array t_array) shape
      in
      let seqInputShapes = List.map get_dummy_shape intermShapes in
      let f arr shape =
        let info = match arr with
          | Array i -> i
          | _ -> failwith "Unexpected intermediate type for reduction"
        in
        let newStrides =
          strides_from_shape shape (Type.sizeof info.elt_type)
        in
        Value.Array {info with array_shape=shape; array_strides=newStrides}
      in
      let seqInputs = List.map2 f interms seqInputShapes in
      List.map Value_to_GenericValue.to_llvm seqInputs
    else
      llvmInterms
  in
  let llvmOutputs : GV.t list =
    allocate_output_generic_values impFn inputShapes
  in
  let params = Array.of_list (seqInputs @ llvmOutputs) in
  let _ = LLE.run_function llvmFn params execution_engine in
  let outputs =
    List.map2 GenericValue_to_Value.of_generic_value llvmOutputs outTypes
  in
  free_scalar_outputs outTypes llvmOutputs;
  (* Not freeing intermediates because GC will take care of them *)
  outputs

let exec_allpairs impFn inputShapes axes array_args llvmFn =
  ()

let adverb (info:(TypedSSA.fn, Ptr.t Value.t list, int list) Adverb.info) =
  assert (info.init = None);
  Printf.printf "[LLVM_Backend.adverb] %s\n%!"
    (Adverb.info_to_str info 
     (fun {TypedSSA.fn_id} -> FnId.to_str fn_id) 
     (fun vs -> String.concat ", " $ List.map Value.to_str vs)
     (fun axes -> String.concat ", " $ List.map string_of_int axes))
  ;
  let axes' = List.map TypedSSA.int32 info.axes in 
  let init' = Option.map (List.map Value.type_of) info.init in 
  let fixedArgs' =  List.map Value.type_of info.fixed_args in 
  let arrayArgs' = List.map Value.type_of info.array_args in 
  let info' = { 
    Adverb.adverb = info.adverb; 
    adverb_fn = info.adverb_fn.TypedSSA.fn_id; 
    fixed_args = fixedArgs'; 
    array_args = arrayArgs'; 
    axes = axes';  
    init = init';  
  } 
  in 
  Printf.printf "[LLVM_Backend.adverb] Making adverb wrapper function...\n%!";  
  let adverbFn =
    AdverbHelpers.mk_adverb_fn info' 
  in
  Printf.printf "Combining args..\n%!"; 
  let allArgValues : Ptr.t Value.t list = info.fixed_args @ info.array_args in
  Printf.printf "Getting types of values\n%!"; 
  let impTypes = List.map ImpType.type_of_value allArgValues in
  Printf.printf "[LLVM_Backend.adverb] Translating SSA fn with imp args %s\n%!"
    (String.concat ", " (List.map ImpType.to_str impTypes));
  let impFn : Imp.fn = SSA_to_Imp.translate_fn adverbFn impTypes in
  let llvmFn = CompiledFunctionCache.compile impFn in
  let inputShapes : Shape.t list = List.map Value.get_shape allArgValues in
  let outputs =
    if do_parallel info then
      match info.adverb with
      | Map -> exec_map impFn inputShapes info.axes info.array_args llvmFn
      | Reduce ->
        exec_reduce impFn inputShapes info.axes info.array_args llvmFn
      | AllPairs ->
        call_imp_fn impFn (info.fixed_args @ info.array_args)
      | Scan -> failwith "Adverb exec function not implemented yet.\n%!"
    else
      call_imp_fn impFn (info.fixed_args @ info.array_args)
  in
  IFDEF DEBUG THEN
    Printf.printf
      "[LLVM_Backend.call_imp_fn] Got function results: %s\n%!"
      (Value.list_to_str ~array_to_str:Value.generic_array_to_str outputs)
    ;
  ENDIF;
  outputs
