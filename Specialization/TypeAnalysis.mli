exception TypeError of string * (SrcInfo.t option)

val type_analysis :
      specializer : (SSA.value -> Signature.t -> SSA.fn) ->
      fn :SSA.fn ->
      signature : Signature.t ->
      (ID.t, Type.t) Hashtbl.t

