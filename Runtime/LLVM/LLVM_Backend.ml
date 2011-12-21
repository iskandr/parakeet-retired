open Imp
open ImpHelpers 
open Llvm
open Imp_to_LLVM

module LLE = Llvm_executionengine.ExecutionEngine  

let memspace_id = HostMemspace.id

let execution_engine = LLE.create Imp_to_LLVM.global_module 

let call fn args =
  let inputTypes = List.map ImpType.type_of_value args in  
  let impFn : Imp.fn = SSA_to_Imp.translate fn inputTypes in 
  let llvmFn : Llvm.llvalue = Imp_to_LLVM.compile_fn impFn in
  let llvmArgs = List.map Value_to_GenericValue.to_llvm args in
  let _ = LLE.run_function llvmFn (Array.of_list llvmArgs) execution_engine in 
  () 

let map ~axes ~fn ~fixed args =
  (*let fn : SSA.fn  = { 
    SSA.fn_input_types = []; 
    fn_output_types = []; 
    body = Block.empty; 
    tenv = ID.Map.empty; 
    fn_id = FnId.of_int 0;
  } *)
  
  (*let fn = SSA_to_Imp.translate fn in*)
	assert false

let reduce ~axes ~fn ~fixed ?init args = assert false
 
let scan ~axes ~fn ~fixed ?init args = assert false
 
let all_pairs ~axes ~fn ~fixed x y = assert false
 
let array_op p args = assert false
