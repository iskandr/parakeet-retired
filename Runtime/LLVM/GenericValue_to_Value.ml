open Base
open ParNum
open Llvm_executionengine

let generic_to_parnum (g_val:GenericValue.t) (impt:Type.elt_t) : ParNum.t = match impt with
  | Type.BoolT -> Bool (GenericValue.as_int g_val <> 0)
  | Type.CharT -> Char (Char.chr (GenericValue.as_int g_val))
  | Type.Int16T -> Int16 (GenericValue.as_int g_val)
  | Type.Int32T -> Int32 (GenericValue.as_int32 g_val)
  | Type.Int64T -> Int64 (GenericValue.as_int64 g_val)
  | Type.Float32T -> Float32(GenericValue.as_float LLVM_Types.float32_t g_val)
  | Type.Float64T -> Float64(GenericValue.as_float LLVM_Types.float64_t g_val)
  | _ -> assert false

let generic_to_imp (g_val:GenericValue.t) = function 
  | ImpType.ScalarT t -> generic_to_parnum g_val t
  | ImpType.ArrayT _ -> assert false
  | _ -> assert false
