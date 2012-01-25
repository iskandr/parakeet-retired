open Base
open ParNum 
open Llvm
open LLVM_Types

let parnum_to_llvm = function
  | Bool b -> const_int int16_t (if b then 1 else 0)
  | Char c -> const_int int16_t (Char.code c)
  | Int16 i -> const_int int16_t i
  | Int32 i32 -> const_int int32_t (Int32.to_int i32)
  (*  | Int32 i32 -> const_of_int64 int32_t (Int64.of_int32 i32) true*)
  | Int64 i64 -> const_of_int64 int64_t i64 true
  | Float32 f -> const_float float32_t f
  | Float64 f -> const_float float64_t f
  | _ -> assert false

let to_llvm = function
  | Value.Scalar s -> parnum_to_llvm s
  | Value.Array a -> assert false
  | _ -> assert false
