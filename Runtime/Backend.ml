type data = Ptr.t Value.t

module type S = sig
  val memspace_id : MemId.t

  val call : TypedSSA.fn -> data list -> data list

  val adverb :
    (TypedSSA.fn, data list, int list) Adverb.t -> data list -> data list

  val array_op : Prim.array_op -> data list -> data list
end
