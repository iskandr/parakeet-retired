(* pp: -parser o pa_macro.cmo *)

open Base
open Llvm_executionengine
open ParNum
open Ptr
open Type
open Value

(* TODO: Everything in this module assumes we're on a platform with: *)
(*       1. 32-bit ints *)
(*       2. 64-bit pointers *)

let pad_to size alignment = (size + alignment - 1) / alignment

let int16 (i:int) = GenericValue.of_int LLVM_Types.int16_t i
let int32 (i:Int32.t) = GenericValue.of_int32 LLVM_Types.int32_t i
let int64 (i:Int64.t) = GenericValue.of_int64 LLVM_Types.int64_t i
let float32 f = GenericValue.of_float LLVM_Types.float32_t f
let float64 f = GenericValue.of_float LLVM_Types.float64_t f

let array_cache : (Int64.t Value.t, GenericValue.t) Hashtbl.t =
  Hashtbl.create 127

let parnum_to_generic = function
  | Bool b -> if b then int16 1 else int16 0
  | Char c -> int16 (Char.code c)
  | Int16 i -> int16 i
  | Int32 i -> int32 i
  | Int64 i -> int64 i
  | Float32 f -> float32 f
  | Float64 f -> float64 f
  | _ -> assert false

let rec to_llvm value =
  match value with
  | Value.Scalar s -> parnum_to_generic s
  | Value.Array a ->
    let addr_value = Value.map Ptr.addr value in
    (match Hashtbl.find_option array_cache addr_value with
      | Some gv_ptr ->
        gv_ptr
      | None ->
        let ptr = HostMemspace.malloc (8 + 8 + 8) in
        let cshape = Array.to_c_int_array (Shape.to_array a.array_shape) in
        let cstrides = Array.to_c_int_array a.array_strides in
        HostMemspace.set_int64 ptr 0 a.data.addr;
        HostMemspace.set_int64 ptr 1 cshape;
        HostMemspace.set_int64 ptr 2 cstrides;
        let gv_ptr = int64 ptr in
        Hashtbl.add array_cache addr_value gv_ptr;
        gv_ptr)
  | Value.Explode (scalar, shape) ->
	  let ptr = HostMemspace.malloc (8 + 8) in
	  let cshape = Array.to_c_int_array (Shape.to_array shape) in
	  (match ParNum.type_of scalar with
	    | Int32T -> HostMemspace.set_int32 ptr 0 (ParNum.to_int32 scalar)
	    | Int64T -> HostMemspace.set_int64 ptr 0 (ParNum.to_int64 scalar)
	    | Float32T -> HostMemspace.set_float32 ptr 0 (ParNum.to_float scalar)
	    | Float64T -> HostMemspace.set_float64 ptr 0 (ParNum.to_float scalar)
	    | _ -> failwith "Unsupported scalar type"
	  );
	  HostMemspace.set_int64 ptr 1 cshape;
	  int64 ptr
  | Value.Rotate (v, dim, offset) ->
	  let ptr : Int64.t = HostMemspace.malloc (8 + 4 + 4) in
	  let a : GenericValue.t = to_llvm v in
	  HostMemspace.set_int64 ptr 0 (GenericValue.as_int64 a);
	  HostMemspace.set_int32 ptr 2 (Int32.of_int dim);
	  HostMemspace.set_int32 ptr 3 (Int32.of_int offset);
	  int64 ptr
  | Value.Shift (v, dim, offset, default) ->
	  let el_t = ParNum.type_of default in
	  let el_size = Type.sizeof el_t in
	  let mem_size = pad_to (8 + 4 + 4 + el_size) 8 in
	  let ptr = HostMemspace.malloc mem_size in
	  let a = to_llvm v in
	  (* As above, we treat int32s as "half-int64s", etc., in order to get *)
	  (* the 64-bit C functions to work.  I'm uncertain that our 32-bit float *)
	  (* support works, as it seems we use a 64-bit OCaml float to store our *)
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
  | Value.FixDim (v, dim, idx) ->
    (* TODO: This is wasteful.  It's unnecessary to actually malloc new *)
    (*       shapes.  In addition, if we have multiple slices into the same *)
    (*       base array, we generate multiple structs for that array in *)
    (*       addition to the waste for the slice itself. Should really clean *)
    (*       up later. *)
    let ptr = HostMemspace.malloc (8 + 8 + 8) in
    let a = to_llvm v in
    let a_ll_ptr = GenericValue.as_int64 a in
    let a_ll_data = HostMemspace.get_int64 a_ll_ptr 0 in
    let array_t = get_underlying_array v in
    let el_t = array_t.elt_type in
    let el_size = sizeof el_t in
    let ashape = (Shape.to_array array_t.array_shape) in
    let astrides = array_t.array_strides in
    let dim_stride = astrides.(dim) in
    let addr = Int64.add a_ll_ptr (Int64.of_int (el_size * dim_stride)) in
    let new_rank = Array.length ashape - 1 in
    let new_shape = Array.make new_rank 0 in
    Array.blit ashape 0 new_shape 0 (dim - 1);
    Array.blit ashape dim new_shape (dim - 1) (new_rank - dim);
    let new_strides = Array.make new_rank 0 in
    Array.blit astrides 0 new_strides 0 (dim - 1);
    Array.blit astrides dim new_strides (dim - 1) (new_rank - dim);
    let ll_shape = Array.to_c_int_array new_shape in
    let ll_strides = Array.to_c_int_array new_strides in
    HostMemspace.set_int64 ptr 0 addr;
    HostMemspace.set_int64 ptr 1 ll_shape;
    HostMemspace.set_int64 ptr 2 ll_strides;
    int64 ptr
  | Value.Slice (v, dim, start, stop) ->
	  (* TODO: This is wasteful.  It's unnecessary to actually malloc new *)
    (*       shapes.  In addition, if we have multiple slices into the same *)
    (*       base array, we generate multiple structs for that array in *)
    (*       addition to the waste for the slice itself. Should really clean *)
    (*       up later. *)
	  let ptr = HostMemspace.malloc (8 + 8 + 8) in
    let a = to_llvm v in
    let a_ll_ptr = GenericValue.as_int64 a in
    let a_ll_data = HostMemspace.get_int64 a_ll_ptr 0 in
    let a_ll_strides = HostMemspace.get_int64 a_ll_ptr 2 in
    let array_t = get_underlying_array v in
    let ashape = array_t.array_shape in
    let astrides = array_t.array_strides in
    let dim_stride = astrides.(dim) in
    let addr = Int64.add a_ll_data (Int64.of_int (start * dim_stride)) in
    let slice_shape = Array.copy (Shape.to_array ashape) in
    Array.set slice_shape dim (stop - start);
	  HostMemspace.set_int64 ptr 0 addr;
	  HostMemspace.set_int64 ptr 1 (Array.to_c_int_array slice_shape);
	  HostMemspace.set_int64 ptr 2 a_ll_strides;
	  int64 ptr
  | Value.Range (start, stop, step) ->
	  let ptr = HostMemspace.malloc (pad_to (4 + 4 + 4) 8) in
	  HostMemspace.set_int32 ptr 0 (Int32.of_int start);
	  HostMemspace.set_int32 ptr 1 (Int32.of_int stop);
	  HostMemspace.set_int32 ptr 2 (Int32.of_int step);
	  int64 ptr

(* acts like to_llvm but maps scalars to their addresses *)
let to_llvm_pointer = function
  | Value.Scalar s -> Obj.magic s
  | other -> to_llvm other

(* TODO: we really probably want to delete values so as to be able to clean *)
(*       up the array_cache *)
let rec delete_llvm_ptr ptr = function
  | ImpType.ScalarT _ -> ()
  | ImpType.ArrayT _ ->
    let shapeptr = HostMemspace.get_int64 ptr 1 in
    let strideptr = HostMemspace.get_int64 ptr 2 in
    HostMemspace.free shapeptr;
    HostMemspace.free strideptr;
    HostMemspace.free ptr
  | ImpType.ExplodeT _ ->
    let shapeptr = HostMemspace.get_int64 ptr 1 in
    HostMemspace.free shapeptr;
    HostMemspace.free ptr
  | ImpType.RotateT _
  | ImpType.ShiftT _
  | ImpType.SliceT _ ->
    let data = HostMemspace.get_int64 ptr 0 in
    (*delete_llvm_ptr data*)
    HostMemspace.free ptr;
    failwith "Don't know how to clean up slice memory"
  | ImpType.RangeT _ -> HostMemspace.free ptr

(* Note: this doesn't delete the data, only the gv struct *)
let delete_llvm_gv (gv:GenericValue.t) (impt:ImpType.t) =
  let ptr = GenericValue.as_int64 gv in
  delete_llvm_ptr ptr impt
