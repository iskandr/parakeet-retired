open Base 
open Llvm

let context = global_context ()

let parnum_to_generic = function
  | Bool b -> 
  	if b then of_int 1 8
  	else of_int 0 8
  | Char c -> of_int (Char.code c) 8
  | Int16 i -> of_int i 16
  | Int32 i -> of_int32 i 32
  | Int64 i -> of_int64 i 64
  | Float32 f -> of_float (float_type context) f
  | Float64 f -> of_float (double_type context) f
  | _ -> assert false

let to_llvm = function
  | Value.Scalar s -> parnum_to_generic s
  | Value.Array a -> assert false
  | _ -> assert false
