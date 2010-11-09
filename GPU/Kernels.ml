open Cuda

(* Index functions *)
(* Note - these functions work for 1D or 2D inputs, since our flattened *)
(*        representation for 1D is the same as 2D with 1-element inner vecs *)
external bind_index_idxs_tex : GpuPtr.t -> int -> unit
  = "ocaml_bind_index_idxs_tex"
external bind_index_int_vecs_tex : GpuPtr.t -> int -> unit
  = "ocaml_bind_index_int_vecs_tex"
external unbind_index_int_vecs_tex : unit -> unit
  = "ocaml_unbind_index_int_vecs_tex"
external bind_index_float_vecs_tex : GpuPtr.t -> int -> unit
  = "ocaml_bind_index_float_vecs_tex"
external unbind_index_float_vecs_tex : unit -> unit
  = "ocaml_unbind_index_float_vecs_tex"
external index_int : int -> int -> int -> GpuPtr.t -> unit
  = "ocaml_index_int"
external index_float : int -> int -> int -> GpuPtr.t -> unit
  = "ocaml_index_float"

(* Where functions *)
external bind_where_tex : GpuPtr.t -> int -> unit = "ocaml_bind_where_tex"
external unbind_where_tex : unit -> unit = "ocaml_unbind_where_tex"
external where_tex : int -> GpuPtr.t -> unit = "ocaml_where_tex"
