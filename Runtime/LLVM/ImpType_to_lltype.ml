open Type
open ImpType 
open Llvm 

let context = Llvm.global_context()
let int_ptr_type  = Llvm.pointer_type i32_type 
 
let scalar_to_lltype = function 
  | BoolT 
  | CharT
  | Int16T
  | Int32T -> i32_type 
  | Int64T -> i64_type 
  | Float32T -> float_type 
  | Float64T -> double_type

let rec to_lltype = function 
  | ScalarT eltT -> scalar_to_lltype eltT
  | ArrayT (eltT, _) ->
     let data = Llvm.pointer_type  (scalar_to_lltype eltT) in 
     Llvm.struct_type context [|data; int_ptr_type; int_ptr_type|]
       
  | ShiftT t ->
     let nested = to_lltype t in 
     Llvm.struct_type context [|nested; i32_type; i32_type; i32_type|]
    
      