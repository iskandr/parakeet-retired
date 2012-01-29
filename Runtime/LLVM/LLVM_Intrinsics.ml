open Llvm
open LLVM_Types

module type LLVM_INTRINSICS = sig
  val m : llmodule
end

module MkIntrinsics(I : LLVM_INTRINSICS) = struct
  let sqrt32 =
    declare_function "llvm.sqrt.f32" (function_type float32_t [|float32_t|]) I.m
  let sqrt64 =
    declare_function "llvm.sqrt.f64" (function_type float64_t [|float64_t|]) I.m
  let powi32 =
    declare_function "llvm.powi.f32"
                     (function_type float32_t [|float32_t;int32_t|])
                     I.m
  let powi64 =
    declare_function "llvm.powi.f64"
                     (function_type float64_t [|float64_t;int32_t|])
                     I.m
  let pow32 =
    declare_function "llvm.pow.f32"
                     (function_type float32_t [|float32_t;float32_t|])
                     I.m
  let pow64 =
    declare_function "llvm.pow.f64"
                     (function_type float64_t [|float64_t;float64_t|])
                     I.m
  let exp32 =
    declare_function "llvm.exp.f32" (function_type float32_t [|float32_t|]) I.m
  let exp64 =
    declare_function "llvm.exp.f64" (function_type float64_t [|float64_t|]) I.m
  let log32 =
    declare_function "llvm.log.f32" (function_type float32_t [|float32_t|]) I.m
  let log64 =
    declare_function "llvm.log.f64" (function_type float64_t [|float64_t|]) I.m
  let sin32 =
    declare_function "llvm.sin.f32" (function_type float32_t [|float32_t|]) I.m
  let sin64 =
    declare_function "llvm.sin.f64" (function_type float64_t [|float64_t|]) I.m
  let cos32 =
    declare_function "llvm.cos.f32" (function_type float32_t [|float32_t|]) I.m
  let cos64 =
    declare_function "llvm.cos.f64" (function_type float64_t [|float64_t|]) I.m
end

