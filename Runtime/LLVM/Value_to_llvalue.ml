open Base 
open Llvm

let context = global_context ()

let parnum_to_llvm = function
  | Bool b -> const_int i32_type context b 
  | Char c -> const_int i32_type context c
  | Int16 i -> const_int i32_type context i
  | Int32 i -> const_int i32_type context i
  | Int64 i -> const_int i64_type context i
  | Float32 f -> const_float float_type context f
  | Float64 f -> const_float double_type context f
  | _ -> assert false

let to_llvm = function
  | Value.Scalar s -> parnum_to_llvm s
  | Value.Array a -> assert false
  | _ -> assert false
