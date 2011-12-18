

val int32_t : Llvm.lltype
val int64_t : Llvm.lltype 
val float32_t  : Llvm.lltype
val float64_t  : Llvm.lltype 

val parnum_to_llvm : ParNum.t -> Llvm.llvalue 

val to_llvm : 'a Value.t -> Llvm.llvalue   
  