type t = Map | Reduce | Scan | AllPairs

val to_str : t -> string


type ('a,'b,'c) info = {
  adverb : t;
  adverb_fn : 'a;
  fixed_args : 'b;
  init : 'b option;
  axes : 'c;
}

val apply_to_fields :
  ('a->'d) -> ('b -> 'e) -> ('c -> 'f) -> ('a,'b, 'c) info -> ('d, 'e, 'f) info


val info_to_str :
  ('a->string) -> ('b->string) -> ('c -> string) -> ('a,'b,'c) info -> string