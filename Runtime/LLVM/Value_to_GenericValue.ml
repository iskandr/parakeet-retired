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

let rec to_llvm = function
  | Value.Scalar s -> parnum_to_generic s
  | Value.Array a -> (
      let ptr = HostMemspace.malloc (8 + 8 + 8) in
      let cshape = Array.to_c_int_array (Shape.to_array a.array_shape) in
      let cstrides = Array.to_c_int_array a.array_strides in
      HostMemspace.set_int64 ptr 0 a.data;
      HostMemspace.set_int64 ptr 1 cshape;
      HostMemspace.set_int64 ptr 2 cstrides;
      ptr)
  | Value.Explode (scalar, shape) -> (
      
  | Value.Rotate (v, dim, offset) -> (
      let ptr = HostMemspace.malloc (8 + 4 + 4) in
      (* Note: the following call makes use of the knowledge that a pointer *)
      (*       is twice as long as an int *)
      let a = to_llvm v in
      HostMemspace.set_int64 ptr 0 a;
      HostMemspace.set_int32 ptr 2 Int32.of_int dim;
      HostMemspace.set_int32 ptr 3 Int32.of_int offset;
      ptr)
  | Value.Shift (v, dim, offset, default) -> (
      let el_t = ParNum.type_of default in
      let el_size = Type.sizeof el_t in
      (* The following ensures that the struct is 8-byte aligned, as all C *)
      (* structs need to be on 64-bit platforms. *)
      let mem_size = (8 + 4 + 4 + el_size + 7) / 8 in
      let ptr = HostMemspace.malloc mem_size in
      let a = to_llvm v in
      (* As above, we treat int32s as "half-int64s", etc., in order to get *)
      (* the 64-bit C functions to work.  I'm uncertain that our 32-bit float *)
      (* support works, as it seems we use a 63-bit OCaml float to store our *)
      (* 32-bit C floats, and I don't see how we ever make that fit. *)
      HostMemspace.set_int64 ptr 0 a;
      HostMemspace.set_int32 ptr 2 dim;
      HostMemspace.set_int32 ptr 3 offset;
      (match el_t with
        | Int32T -> HostMemspace.set_int32 ptr 4 (ParNum.to_int32 default)
        | Int64T -> HostMemspace.set_int64 ptr 2 (ParNum.to_int64 default)
        | Float32T -> HostMemspace.set_float32 ptr 4 (ParNum.to_float default)
        | Float64T -> HostMemspace.set_float64 2 (ParNum.to_float default)
        | _ -> failwith "Unsupported array element type for LLVM conversion");
      ptr)
  | Value.Slice (v, dim, start, stop) -> (
      (* Need to pad the struct to make it 8-byte aligned. *)
      let ptr = HostMemspace.malloc (8 + 4 + 4 + 4 + 4) in
      let a = to_llvm v in
      HostMemspace.set_int64 ptr 0 a;
      HostMemspace.set_int32 ptr 2 dim;
      HostMemspace.set_int32 ptr 3 start;
      HostMemspace.set_int32 ptr 4 stop;
      ptr)
  | Value.Range (start, stop, ?step=1) -> assert false (* what to do? *)
  | _ -> assert false

let delete_llvm_gv gv = function
  | ImpType.ScalarT -> ()
  | ImpType.ArrayT
  | ImpType.ShiftT -> HostMemspace.free gv
  | _ -> failwith "Unsupported ImpType for LLVM deletion"
