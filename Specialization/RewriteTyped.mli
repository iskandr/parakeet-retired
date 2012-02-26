
val rewrite_typed :
      tenv : (ID.t, Type.t) Hashtbl.t ->
        specializer : (UntypedSSA.value -> Signature.t -> TypedSSA.fn) ->
          fn:UntypedSSA.fn -> 
            signature:Signature.t -> 
              TypedSSA.fn
