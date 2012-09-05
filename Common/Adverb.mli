type t = Map | Reduce | Scan 


type ('a, 'b) init = 
  | IniFn of 'a 
  | InitValues of 'b 

type ('a, 'b, 'c) adverb_info { 
  adverb : t; 
  fn : 'a; 
  fixed_args : 'b;
  array_args : 'b; 
  axes : 'c; 
  (* initialize scans and reductions either with value or function mapping
     array element to acc 
  *) 
  init : ('a, 'b) init option; 
  (* given two accumulators, combine them into a single new accumulator
     if this function isn't given we can't parallelize reductions & scans 
  *) 
  combine_fn : 'a option;  
}
(*
type ('a, 'b, 'c) map_info = { 
  map_fn : 'a; 
  map_fixed_args : 'b; 
  map_array_args : 'b; 
  map_axes : 'c;   
}


type ('a, 'b) init = 
  | IniFn of 'a 
  | InitValues of 'b 

type ('a, 'b, 'c) reduce_info = { 
  reduce_fn : 'a; 
  combine_fn : 'a;
  reduce_fixed_args : 'b;
  init : ('a, 'b) init; 
  reduce_array_args : 'b; 
  reduce_axes : 'c;
  keep_prefix : bool; 
}

type adverb = Map of map_info | Reduce of reduce_info
*) 
val apply_to_fields :
  ('a, 'b, 'c) info -> 
    fn:('a->'d) -> values:('b -> 'e) -> axes:('c -> 'f) -> 
      ('d, 'e, 'f) info


val info_to_str :
  ('a, 'b, 'c) info -> ('a->string) -> ('b->string) -> ('c -> string) -> string
