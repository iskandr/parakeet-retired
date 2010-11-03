open Cuda

external thrust_prefix_sum_int : CuModulePtr.t -> int -> CuModulePtr.t -> unit =
  "ocaml_thrust_prefix_sum_int"

external thrust_prefix_sum_float : CuModulePtr.t -> int -> CuModulePtr.t -> unit
  = "ocaml_thrust_prefix_sum_float"
