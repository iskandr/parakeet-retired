open Imp
open ImpHelpers
open Imp_to_LLVM
open Llvm

module LLE = Llvm_executionengine.ExecutionEngine
module GV = Llvm_executionengine.GenericValue

let _ = Llvm_executionengine.initialize_native_target()
let execution_engine = LLE.create Imp_to_LLVM.global_module

let optimize_module llvmModule llvmFn : unit =
  let the_fpm = PassManager.create_function llvmModule in
  (* Set up the optimizer pipeline.  Start with registering info about how the
   * target lays out data structures. *)
  Llvm_target.TargetData.add (LLE.target_data execution_engine) the_fpm;

  (* Promote allocas to registers. *)
  Llvm_scalar_opts.add_memory_to_register_promotion the_fpm;
  let modified = PassManager.run_function llvmFn the_fpm in
  Printf.printf "Optimizer modified code: %b\n" modified;
  let _ : bool = PassManager.finalize the_fpm in
  PassManager.dispose the_fpm

let memspace_id = HostMemspace.id

let strides_from_shape shape =
  let rank = Shape.rank shape in
  let strides = Array.create rank 1 in
  for i = rank - 2 downto 0 do
    strides.(i) <- strides.(i+1) * (Shape.get shape (i+1))
  done;
  strides

let allocate_output impT (shape:Shape.t) : GV.t =
  match impT with
  | ImpType.ScalarT eltT ->
    GV.of_int64 LLVM_Types.int64_t (HostMemspace.malloc (Type.sizeof eltT))
  | ImpType.ArrayT (eltT, rank) ->
    let eltSize : int = Type.sizeof eltT in
    let nelts = Shape.nelts shape in
    let arrayVal = Value.Array {
      Value.data = Mem.alloc HostMemspace.id (nelts * eltSize);
      array_type = Type.ArrayT(eltT, rank);
      elt_type = eltT;
      array_shape = shape;
      array_strides = strides_from_shape shape;
    }
    in
    Value_to_GenericValue.to_llvm arrayVal
  | other ->
    failwith ("Can't create output array of type" ^ (ImpType.to_str other))

let allocate_outputs impFn args =
  let impTypes = Imp.output_types impFn in
  let inputShapes : Shape.t list = List.map Value.get_shape args in
  (* compute output shapes from input shapes *)
  let outputShapes = ShapeEval.get_call_output_shapes impFn inputShapes in
  List.map2 allocate_output impTypes outputShapes

let free_scalar_output impT (gv:GV.t) : unit =
  if ImpType.is_scalar impT then HostMemspace.free (GV.as_int64 gv)

let free_scalar_outputs impTypes gvs = List.map2 free_scalar_output impTypes gvs

let call_imp_fn (impFn:Imp.fn) (args:Ptr.t Value.t list) : Ptr.t Value.t list =
  let llvmFn : Llvm.llvalue = Imp_to_LLVM.compile_fn impFn in
  optimize_module Imp_to_LLVM.global_module llvmFn;
  print_endline  "[LLVM_Backend.call_imp_fn] Generated LLVM function";
  Llvm.dump_value llvmFn;
  Llvm_analysis.assert_valid_function llvmFn;
  Printf.printf "Preallocating outputs...\n";
  let llvmInputs : GV.t list = List.map Value_to_GenericValue.to_llvm args in
  let llvmOutputs = allocate_outputs impFn args in
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
  Printf.printf " :: deallocating output cells\n%!";
  free_scalar_outputs impOutputTypes llvmOutputs;
  Printf.printf "[LLVM_Backend.call_imp_fn] Got function results: %s\n%!"
                (Value.list_to_str outputs);
  outputs

let call (fn:SSA.fn) args =
  let inputTypes = List.map ImpType.type_of_value args in
  let impFn : Imp.fn = SSA_to_Imp.translate_fn fn inputTypes in
  Printf.printf "\n[LLVM_Backend.call] Created Imp function: %s\n%!"
                (Imp.fn_to_str impFn);
  call_imp_fn impFn args

let map ~axes ~fn ~fixed args =
  (*let fn : SSA.fn  = {
    SSA.fn_input_types = [];
    fn_output_types = [];
    body = Block.empty;
    tenv = ID.Map.empty;
    fn_id = FnId.of_int 0;
  } *)

  (*let fn = SSA_to_Imp.translate fn in*)
  (*let outType = ImpType.ArrayT (Type.Float64T, 100) in
  let gv = call fn args in
  let value = GenericValue_to_Value.of_generic_value gv outType in
  *)assert false

let reduce ~axes ~fn ~fixed ?init args = assert false

let scan ~axes ~fn ~fixed ?init args = assert false

let all_pairs ~axes ~fn ~fixed x y = assert false

let array_op p args = assert false
