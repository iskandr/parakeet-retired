type t = Map | Reduce | Scan | AllPairs

val has_accumulator : t -> bool
val to_str : t -> string


type ('a,'b,'c) info = {
  adverb : t;
  adverb_fn : 'a;
  fixed_args : 'b;
  init : 'b option;
  axes : 'c;
  array_args : 'b; 
}

val apply_to_fields :
  ('a, 'b, 'c) info -> 
    fn:('a->'d) -> values:('b -> 'e) -> axes:('c -> 'f) -> 
      ('d, 'e, 'f) info


val info_to_str :
  ('a, 'b, 'c) info -> ('a->string) -> ('b->string) -> ('c -> string) -> string
