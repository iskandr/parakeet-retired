open Imp
open ImpHelpers
open Imp_to_LLVM 
open Llvm

module LLE = Llvm_executionengine.ExecutionEngine

let memspace_id = HostMemspace.id

let execution_engine = LLE.create Imp_to_LLVM.global_module

let call_imp_fn (impFn : Imp.fn) (args : Ptr.t Value.t list) : Ptr.t Value.t list = 
  let llvmFn : Llvm.llvalue = Imp_to_LLVM.compile_fn impFn in
  print_endline  "[LLVM_Backend.call_imp_fn] Generated LLVM function";
  Llvm.dump_value llvmFn;
  Printf.printf "[LLVM_Backend.call_imp_fn] Running function with arguments %s\n" (Value.list_to_str args); 
  let llvmArgs = List.map Value_to_GenericValue.to_llvm args in
  let gv = LLE.run_function llvmFn (Array.of_list llvmArgs) execution_engine in
  let gvs = [gv] in
  let outTypes = Imp.output_types impFn in  
  let results = 
    List.map2 GenericValue_to_Value.of_generic_value gvs outTypes
  in 
  Printf.printf "Got function results: %s" (Value.list_to_str results);  
  results  

let call (fn:SSA.fn) args =
  let inputTypes = List.map ImpType.type_of_value args in
  let impFn : Imp.fn = SSA_to_Imp.translate fn inputTypes in
  Printf.printf "\n[LLVM_Backend.call] Created Imp function: %s\n" (Imp.fn_to_str impFn);  
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
