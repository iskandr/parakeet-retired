open Base
open Type
open ParNum
open Ptr  
open Value 
open Llvm_executionengine 

let pad_to size alignment = (size + alignment - 1) / alignment

let int16 (i:int) = GenericValue.of_int LLVM_Types.int16_t i 
let int32 (i:Int32.t) = GenericValue.of_int32 LLVM_Types.int32_t i 
let int64 (i:Int64.t) = GenericValue.of_int64 LLVM_Types.int64_t i
let float32 f = GenericValue.of_float LLVM_Types.float32_t f
let float64 f = GenericValue.of_float LLVM_Types.float64_t f 

let parnum_to_generic = function
  | Bool b -> if b then int16 1 else int16 0 
  | Char c -> int16 (Char.code c)
  | Int16 i -> int16 i
  | Int32 i -> int32 i
  | Int64 i -> int64 i
  | Float32 f -> float32 f
  | Float64 f -> float64 f 
  | _ -> assert false

let rec to_llvm = function
  | Value.Scalar s -> parnum_to_generic s
  | Value.Array a -> 
      let ptr = HostMemspace.malloc (8 + 8 + 8) in
      let cshape = Array.to_c_int_array (Shape.to_array a.array_shape) in
      let cstrides = Array.to_c_int_array a.array_strides in
      HostMemspace.set_int64 ptr 0 a.data.addr;
      HostMemspace.set_int64 ptr 1 cshape;
      HostMemspace.set_int64 ptr 2 cstrides;
      int64 ptr
  | Value.Explode (scalar, shape) -> 
      let ptr = HostMemspace.malloc (8 + 8) in
      let cshape = Array.to_c_int_array (Shape.to_array shape) in
      (match ParNum.type_of scalar with
        | Int32T -> HostMemspace.set_int32 ptr 0 (ParNum.to_int32 scalar)
        | Int64T -> HostMemspace.set_int64 ptr 0 (ParNum.to_int64 scalar)
        | Float32T -> HostMemspace.set_float32 ptr 0 (ParNum.to_float scalar)
        | Float64T -> HostMemspace.set_float64 ptr 0 (ParNum.to_float scalar)
      );
      HostMemspace.set_int64 ptr 1 cshape;
      int64 ptr
  | Value.Rotate (v, dim, offset) -> 
      let ptr : Int64.t = HostMemspace.malloc (8 + 4 + 4) in
      (* Note: the following call makes use of the knowledge that a pointer *)
      (*       is twice as long as an int *)
      let a : GenericValue.t = to_llvm v in
      HostMemspace.set_int64 ptr 0 (GenericValue.as_int64 a);
      HostMemspace.set_int32 ptr 2 (Int32.of_int dim);
      HostMemspace.set_int32 ptr 3 (Int32.of_int offset);
      int64 ptr
  | Value.Shift (v, dim, offset, default) -> 
      let el_t = ParNum.type_of default in
      let el_size = Type.sizeof el_t in
      (* The following ensures that the struct is a multiple of 8 bytes, as *)
      (* all C structs need to be on 64-bit platforms. *)
      let mem_size = pad_to (8 + 4 + 4 + el_size) 8 in
      let ptr = HostMemspace.malloc mem_size in
      let a = to_llvm v in
      (* As above, we treat int32s as "half-int64s", etc., in order to get *)
      (* the 64-bit C functions to work.  I'm uncertain that our 32-bit float *)
      (* support works, as it seems we use a 63-bit OCaml float to store our *)
      (* 32-bit C floats, and I don't see how we ever make that fit. *)
      HostMemspace.set_int64 ptr 0 (GenericValue.as_int64 a);
      HostMemspace.set_int32 ptr 2 (Int32.of_int dim);
      HostMemspace.set_int32 ptr 3 (Int32.of_int offset);
      (match el_t with
        | Int32T -> HostMemspace.set_int32 ptr 4 (ParNum.to_int32 default)
        | Int64T -> HostMemspace.set_int64 ptr 2 (ParNum.to_int64 default)
        | Float32T -> HostMemspace.set_float32 ptr 4 (ParNum.to_float default)
        | Float64T -> HostMemspace.set_float64 ptr 2 (ParNum.to_float default)
        | _ -> failwith "Unsupported array element type for LLVM conversion");
      int64 ptr
  | Value.Slice (v, dim, start, stop) -> 
      let ptr = HostMemspace.malloc (pad_to (8 + 4 + 4 + 4) 8) in
      let a = to_llvm v in
      HostMemspace.set_int64 ptr 0 (GenericValue.as_int64 a);
      HostMemspace.set_int32 ptr 2 (Int32.of_int dim);
      HostMemspace.set_int32 ptr 3 (Int32.of_int start);
      HostMemspace.set_int32 ptr 4 (Int32.of_int stop);
      int64 ptr
  | Value.Range (start, stop, step) -> 
      let ptr = HostMemspace.malloc (pad_to (4 + 4 + 4) 8) in
      HostMemspace.set_int32 ptr 0 (Int32.of_int start);
      HostMemspace.set_int32 ptr 1 (Int32.of_int stop);
      HostMemspace.set_int32 ptr 2 (Int32.of_int step);
      int64 ptr
  | _ -> assert false

let rec delete_llvm_gv (gv:GenericValue.t) = function
  | ImpType.ScalarT _ -> ()
  | ImpType.ArrayT _
  | ImpType.ShiftT _ -> HostMemspace.free (GenericValue.as_int64 gv)
  (* MEMORY LEAK!!!! Not recursively destroying the struct elements *)
  | _ -> failwith "Unsupported ImpType for LLVM deletion"
