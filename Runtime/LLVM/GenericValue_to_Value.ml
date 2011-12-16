open Base 
open Llvm

let context = global_context ()

let generic_to_parnum (g_val:GenericValue.t) (impt:ImpType.t) : ParNum.t = match impt with
  | Type.BoolT -> Parnum.Bool (as_int g_val <> 0)
  | Type.CharT -> Parnum.Char (Char.chr (as_int g_val))
  | Type.Int16T -> Parnum.Int16 (as_int g_val)
  | Type.Int32T -> Parnum.Int32 (as_int32 g_val)
  | Type.Int64T -> Parnum.Int64 (as_int64 g_val)
  | Type.Float32T -> Parnum.Float32(as_float (float_type context) g_val)
  | Type.Float64T -> Parnum.Float64(as_float (double_type context) g_val)
  | _ -> assert false

let generic_to_imp (g_val:GenericValue.t) = function 
  | ImpType.ScalarT t -> generic_to_parnum g_val t
  | ImpType.ArrayT _ -> assert false
  | _ -> assert false