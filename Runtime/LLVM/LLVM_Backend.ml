open Imp
open ImpHelpers 
open Llvm
open Imp_to_LLVM

let memspace_id = HostMemspace.id

let map ~axes ~fn ~fixed args =
	(*let fn = SSA_to_Imp.translate fn in
  let _ = Imp_to_LLVM.compile_fn fn in*) 
	assert false

let reduce ~axes ~fn ~fixed ?init args = assert false
 
let scan ~axes ~fn ~fixed ?init args = assert false
 
let all_pairs ~axes ~fn ~fixed x y = assert false
 
let array_op p args = assert false
