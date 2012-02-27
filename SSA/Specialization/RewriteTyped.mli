
val rewrite_typed :
      specializer : (UntypedSSA.value -> Signature.t -> TypedSSA.fn) ->
        fn:UntypedSSA.fn ->
          signature:Signature.t ->
            TypedSSA.fn
