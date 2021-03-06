val void_t : Llvm.lltype

val char_t : Llvm.lltype
val int16_t : Llvm.lltype
val int32_t : Llvm.lltype
val int64_t : Llvm.lltype
val float32_t  : Llvm.lltype
val float64_t  : Llvm.lltype

val char_ptr_t : Llvm.lltype
val int16_ptr_t : Llvm.lltype
val int32_ptr_t : Llvm.lltype
val int64_ptr_t : Llvm.lltype
val float32_ptr_t  : Llvm.lltype
val float64_ptr_t  : Llvm.lltype

val vec4_float32_t : Llvm.lltype
val vec4_int32_t : Llvm.lltype
val vec2_float64_t : Llvm.lltype
val vec2_int64_t : Llvm.lltype

val is_pointer : Llvm.lltype -> bool
val is_scalar : Llvm.lltype -> bool
val replace_pointer_with_int64 : Llvm.lltype -> Llvm.lltype
val replace_pointers : Llvm.lltype list -> Llvm.lltype list


val of_elt_type : Type.elt_t -> Llvm.lltype
val of_imp_type : ImpType.t -> Llvm.lltype