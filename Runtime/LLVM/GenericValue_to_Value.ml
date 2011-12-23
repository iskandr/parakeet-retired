open Base
open Llvm_executionengine
open ParNum
open Value

let generic_to_parnum (g_val:GenericValue.t) (impt:Type.elt_t) : ParNum.t =
  match impt with
  | Type.BoolT -> Bool (GenericValue.as_int g_val <> 0)
  | Type.CharT -> Char (Char.chr (GenericValue.as_int g_val))
  | Type.Int16T -> Int16 (GenericValue.as_int g_val)
  | Type.Int32T -> Int32 (GenericValue.as_int32 g_val)
  | Type.Int64T -> Int64 (GenericValue.as_int64 g_val)
  | Type.Float32T -> Float32 (GenericValue.as_float LLVM_Types.float32_t g_val)
  | Type.Float64T -> Float64 (GenericValue.as_float LLVM_Types.float64_t g_val)
  | _ -> assert false

let generic_to_imp (g_val:GenericValue.t) = function 
  | ImpType.ScalarT t -> Scalar (generic_to_parnum g_val t)
  | ImpType.ArrayT (elt_t, len) ->
    let gv_ptr = GenericValue.as_pointer g_val in
    let data = HostMemspace.get_int64 gv_ptr 0 in
    let shapeptr = HostMemspace.get_int64 gv_ptr 1 in
    let shape = Shape.of_array (Array.from_c_int_array shapeptr len) in
    let strideptr = HostMemspace.get_int64 gv_ptr 2 in
    let strides = Array.from_c_int_array strideptr len in
    Array {data=data; array_type=Type.ArrayT (elt_t, len); elt_type=elt_t;
           array_shape=shape; array_strides=strides}
  | _ -> assert false
