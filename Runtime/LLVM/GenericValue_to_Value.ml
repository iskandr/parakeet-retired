open Base
open Llvm_executionengine
open ParNum
open Value

let int_array_of_addr addr len =
  let res = Array.make len 0 in
  for i = 0 to len - 1 do
    res.(i) <- Int64.to_int (HostMemspace.get_int64 addr i)
  done;
  res

let generic_value_to_parnum (g_val:GenericValue.t) (impt:Type.elt_t) : ParNum.t
  =
  match impt with
  | Type.BoolT -> Bool (GenericValue.as_int g_val <> 0)
  | Type.CharT -> Char (Char.chr (GenericValue.as_int g_val))
  | Type.Int16T -> Int16 (GenericValue.as_int g_val)
  | Type.Int32T -> Int32 (GenericValue.as_int32 g_val)
  | Type.Int64T -> Int64 (GenericValue.as_int64 g_val)
  | Type.Float32T -> Float32 (GenericValue.as_float LLVM_Types.float32_t g_val)
  | Type.Float64T -> Float64 (GenericValue.as_float LLVM_Types.float64_t g_val)
  | _ -> assert false

let of_generic_value ?(boxed_scalars=true) (gv:GenericValue.t) = function
  | ImpType.ScalarT t ->
      if boxed_scalars then
        let addr : Int64.t = GenericValue.as_int64 gv in
        Scalar (HostMemspace.deref_scalar addr t)
      else
        Scalar (generic_value_to_parnum gv t)
  | ImpType.ArrayT (elt_t, len) ->
    let gv_ptr : Int64.t = GenericValue.as_int64 gv in
    let data_addr : Int64.t = HostMemspace.get_int64 gv_ptr 0 in
    let shape_addr : Int64.t = HostMemspace.get_int64 gv_ptr 1 in
    let shape : Shape.t = Shape.of_array (int_array_of_addr shape_addr len) in
    let data_nbytes : int = (Shape.nelts shape) * (Type.sizeof elt_t) in
    let data_ptr : Ptr.t = HostMemspace.mk_host_ptr data_addr data_nbytes in
    let strides_addr : Int64.t = HostMemspace.get_int64 gv_ptr 2 in
    let strides : int array = int_array_of_addr strides_addr len in
    Array {
      data=data_ptr;
      array_type=Type.ArrayT (elt_t, len);
      elt_type=elt_t;
      array_shape=shape;
      array_strides=strides
    }
  | _ -> assert false
