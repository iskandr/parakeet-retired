exception TypeError of string * (SrcInfo.t option)

val infer_scalar_op : Prim.scalar_op -> Type.t list -> Type.t
(* first order array operators take data arguments and return only one value *)
val infer_simple_array_op : Prim.array_op -> Type.t list -> Type.t


(* to which types must inputs be cast for an operator to work? *)
val required_scalar_op_types :
      Prim.scalar_op -> Type.t list -> Type.t list

val infer_num_axes : ?src:SrcInfo.t -> ?axes:'a list -> Type.t list -> int

val check_adverb_error :
  ?src:SrcInfo.t -> Adverb.t -> Type.t list option -> Type.t list -> unit

val infer_adverb_result_types :
  adverb:Adverb.t -> elt_result_types:Type.t list -> num_axes:int -> Type.t list

val type_analysis :
      specializer : (UntypedSSA.value -> Signature.t -> TypedSSA.fn) ->
      fn : UntypedSSA.fn ->
      signature : Signature.t ->
      (ID.t, Type.t) Hashtbl.t

