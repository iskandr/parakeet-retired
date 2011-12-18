open Base
open ParNum 
open Llvm

let context = global_context ()
let int32_t = i32_type context
let int64_t = i64_type context 
let float32_t = float_type context
let float64_t = double_type context 

let parnum_to_llvm = function
  | Bool b -> const_int i32_type (if b then 1 else 0) 
  | Char c -> const_int i32_type (Char.code c)
  | Int16 i -> const_int i32_type i
  | Int32 i32 -> const_of_int32 i32_type (Int64.of_int32 i) true 
  | Int64 i64 -> const_of_int64 i64_type i64 true 
  | Float32 f -> const_float float_type f
  | Float64 f -> const_float double_type f
  | _ -> assert false

let to_llvm = function
  | Value.Scalar s -> parnum_to_llvm s
  | Value.Array a -> assert false
  | _ -> assert false
