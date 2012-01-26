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

let alloc_output = function
  | ImpType.ScalarT eltT -> Value.Scalar (ParNum.zero eltT)
  | _ -> failwith "Can't yet preallocate arrays"

let allocate_output impT : GV.t =
  let eltT = ImpType.elt_type impT in
  let sz : int  = Type.sizeof eltT in
  let ptr : Int64.t = HostMemspace.malloc sz in
  Printf.printf "  Allocated %d-byte output of type %s at addr %LX\n%!"
    sz
    (Type.elt_to_str eltT)
    ptr;
  HostMemspace.set_scalar ptr (ParNum.one eltT);
  Printf.printf "  Stored 0 in memory location\n%!";
  Printf.printf "  Dereferenced value: %s\n%!"
    (ParNum.to_str (HostMemspace.deref_scalar ptr eltT));
  GV.of_int64 LLVM_Types.int64_t ptr

let allocate_outputs impTypes = List.map allocate_output impTypes

let allocate_outputs impTypes = List.map allocate_output impTypes

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
  let impOutputTypes = Imp.output_types impFn in
  let llvmOutputs = allocate_outputs impOutputTypes in
  Printf.printf "[LLVM_Backend.call_imp_fn] Running function\n%!";
  let impInputTypes = Imp.input_types impFn in
  Printf.printf "  -- input params: %s\n%!"
     (Value.list_to_str
       (List.map2 (GenericValue_to_Value.of_generic_value ~boxed_scalars:false)
                  llvmInputs impInputTypes)
     );
  Printf.printf "  -- output params: %s\n%!"
    (Value.list_to_str
      (List.map2 GenericValue_to_Value.of_generic_value
                 llvmOutputs impOutputTypes))
  ;
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
  let impFn : Imp.fn = SSA_to_Imp.translate fn inputTypes in
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
