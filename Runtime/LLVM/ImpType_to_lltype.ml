open LLVM_Types

let scalar_to_lltype = function
  | Type.BoolT
  | Type.CharT
  | Type.Int16T -> int16_t
  | Type.Int32T -> int32_t
  | Type.Int64T -> int64_t
  | Type.Float32T -> float32_t
  | Type.Float64T -> float64_t

let context = Llvm.global_context ()

let rec to_lltype = function
  | ImpType.ScalarT eltT -> scalar_to_lltype eltT
  | ImpType.ArrayT (eltT, _) ->
    let data = Llvm.pointer_type (scalar_to_lltype eltT) in
    let structT = Llvm.struct_type context [|data; int32_ptr_t; int32_ptr_t|] in
    Llvm.pointer_type structT
  | ImpType.ShiftT t ->
    let nested = to_lltype t in
    let structT =
      Llvm.struct_type context [|nested; int32_t; int32_t; int32_t|]
    in
    Llvm.pointer_type structT
