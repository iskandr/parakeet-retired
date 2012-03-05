open Llvm
let context : llcontext = global_context ()

let void_t : lltype = void_type context

let char_t : lltype = i8_type context
let int16_t : lltype = i16_type context
let int32_t : lltype = i32_type context
let int64_t : lltype = i64_type context
let float32_t : lltype = float_type context
let float64_t : lltype = double_type context

let char_ptr_t = Llvm.pointer_type char_t
let int16_ptr_t = Llvm.pointer_type int16_t
let int32_ptr_t = Llvm.pointer_type int32_t
let int64_ptr_t = Llvm.pointer_type int64_t
let float32_ptr_t = Llvm.pointer_type float32_t
let float64_ptr_t = Llvm.pointer_type float64_t

let is_scalar t =
  let kind = Llvm.classify_type t in
  (kind == Llvm.TypeKind.Float) ||
  (kind == Llvm.TypeKind.Double) ||
  (kind == Llvm.TypeKind.Integer)

let is_pointer ptrTy =
  Llvm.classify_type ptrTy == Llvm.TypeKind.Pointer


let replace_pointer_with_int64 ty =
  if is_pointer ty then
    int64_t
 	else
    ty

let replace_pointers tyList =
  List.map replace_pointer_with_int64 tyList


let of_elt_type = function
  | Type.BoolT
  | Type.CharT
  | Type.Int16T -> int16_t
  | Type.Int32T -> int32_t
  | Type.Int64T -> int64_t
  | Type.Float32T -> float32_t
  | Type.Float64T -> float64_t

let rec of_imp_type = function
  | ImpType.ScalarT eltT -> of_elt_type eltT
  | ImpType.PtrT(eltT, _) -> Llvm.pointer_type (of_elt_type eltT)
  | ImpType.VecSliceT (eltT, width) ->
    let scalarT = of_elt_type eltT in
    Llvm.vector_type scalarT width
  | compound ->
    let nestedTypes = Imp.field_types compound in
    let llvmFieldTypes = List.map of_imp_type nestedTypes in
    Llvm.struct_type context (Array.of_list llvmFieldTypes)

let input_type (t:ImpType.t) =
  let llvmT = of_imp_type t in
  if ImpType.is_scalar t then llvmT
  else Llvm.pointer_type llvmT

let output_type (t:ImpType.t) = Llvm.pointer_type (of_imp_type t)
(*
let adjust_output_pointer outTy =
  if is_pointer outTy then
    outTy
  else
    Llvm.pointer_type outTy
*)