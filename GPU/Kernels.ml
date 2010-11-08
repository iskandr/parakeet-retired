open Cuda

external bind_where_tex : GpuPtr.t -> int -> unit = "ocaml_bind_where_tex"
external unbind_where_tex : unit -> unit = "ocaml_unbind_where_tex"

external where_tex : int -> GpuPtr.t -> unit = "ocaml_where_tex"
