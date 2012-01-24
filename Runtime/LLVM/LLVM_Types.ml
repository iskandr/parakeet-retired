open Llvm
let context : llcontext = global_context ()

let void_t : lltype = void_type context

let int16_t : lltype = i16_type context
let int32_t : lltype = i32_type context
let int64_t : lltype = i64_type context
let float32_t : lltype = float_type context
let float64_t : lltype = double_type context

let int16_ptr_t = Llvm.pointer_type int16_t
let int32_ptr_t = Llvm.pointer_type int32_t
let int64_ptr_t = Llvm.pointer_type int64_t
let float32_ptr_t = Llvm.pointer_type float32_t
let float64_ptr_t = Llvm.pointer_type float64_t
