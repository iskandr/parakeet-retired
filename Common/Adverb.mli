type adverb_type = Map | Reduce | Scan 

val adverb_type_to_str : adverb_type -> string 
(*
type 'a init = 
  | InitValues of 'a
  | InitFirstElt 
*)
type ('a, 'b, 'c) t = { 
  adverb_type : adverb_type; 
  fn : 'a; 
  fixed : 'b;
  args : 'b; 
  axes : 'c; 
  (* initialize scans and reductions either with value or function mapping
     array element to acc 
  *) 
  init : 'b option; 
  (* given two accumulators, combine them into a single new accumulator
     if this function isn't given we can't parallelize reductions & scans 
  *) 
  combine : 'a option;  
  
}


val apply_to_fields :
  ('a, 'b, 'c) t -> 
    fn:('a->'d) -> values:('b -> 'e) -> axes:('c -> 'f) -> 
      ('d, 'e, 'f) t


val to_str :
  ('a, 'b, 'c) t -> ('a->string) -> ('b->string) -> ('c -> string) -> string
