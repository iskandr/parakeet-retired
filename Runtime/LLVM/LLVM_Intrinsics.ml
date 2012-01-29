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
end

