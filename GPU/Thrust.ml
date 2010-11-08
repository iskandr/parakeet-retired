open Cuda

external thrust_prefix_sum_int : GpuPtr.t -> int -> GpuPtr.t -> unit
  = "ocaml_thrust_prefix_sum_int"

external thrust_prefix_sum_float : GpuPtr.t -> int -> GpuPtr.t -> unit
  = "ocaml_thrust_prefix_sum_float"
