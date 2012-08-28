
type ('a, 'b, 'c) map_info = { 
  map_fn : 'a; 
  map_fixed_args : 'b; 
  map_array_args : 'b; 
  map_axes : 'c;   
}


type 'a init = 
  | InitFirstElement
  | InitValues of 'a 

type ('a, 'b, 'c) reduce_info = { 
  reduce_fn : 'a; 
  combiner : 'a;
  reduce_fixed_args : 'b;
  init : 'b init; 
  reduce_array_args : 'b; 
  reduce_axes : 'c;
  keep_prefix : bool; 
}

type adverb = Map of map_info | Reduce of reduce_info

val apply_to_fields :
  ('a, 'b, 'c) info -> 
    fn:('a->'d) -> values:('b -> 'e) -> axes:('c -> 'f) -> 
      ('d, 'e, 'f) info


val info_to_str :
  ('a, 'b, 'c) info -> ('a->string) -> ('b->string) -> ('c -> string) -> string
