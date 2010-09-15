(* Object oriented wrapper around Hashtbl which makes for more compact code. 
   Generally use "dict", unless you need to wrap the key space of  
  the underlying hashtbl with a transformation, then use "dict_wrapper" *)

class ['a, 'b, 'c] dict_wrapper :
  ('a, 'b) Base.Hashtbl.t ->
  ('c -> 'a) ->
  ('a -> 'c) ->
  object
    method add : 'c -> 'b -> unit
    method enum : ('c * 'b) Enum.t
    method find : 'c -> 'b
    method iter : ('c -> 'b -> unit) -> unit
    method mem : 'c -> bool
    method remove : 'c -> unit
  end
val id : 'a -> 'a
class ['a, 'b] dict :
  object
    method add : 'a -> 'b -> unit
    method enum : ('a * 'b) Enum.t
    method find : 'a -> 'b
    method iter : ('a -> 'b -> unit) -> unit
    method mem : 'a -> bool
    method remove : 'a -> unit
  end
