open Base 
open Llvm

let context = global_context ()

let generic_to_parnum g_val impt = function match impt with
  | ImpType.bool_t -> as_int g_val <> 0
  | ImpType.char_t -> Char.chr (as_int g_val)
  | ImpType.int16_t -> as_int g_val
  | ImpType.int32_t -> as_int32 g_val
  | ImpType.int64_t -> as_int64 g_val
  | ImpType.float32_t -> as_float (float_type context) g_val
  | ImpType.float64_t -> as_float (double_type context) g_val
  | _ -> assert false

let generic_to_imp g_val = function 
  | ImpType.ScalarT t -> generic_to_parnum g_val t
  | ImpType.ArrayT _ -> assert false
  | _ -> assert false