type adverb_type = Map | Reduce | Scan 


type ('a, 'b) init = 
  | IniFn of 'a 
  | InitValues of 'b
  | InitFirstElt  

type ('a, 'b, 'c) t = { 
  adverb : adverb_type; 
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


val apply_to_fields :
  ('a, 'b, 'c) t -> 
    fn:('a->'d) -> values:('b -> 'e) -> axes:('c -> 'f) -> 
      ('d, 'e, 'f) t


val info_to_str :
  ('a, 'b, 'c) t -> ('a->string) -> ('b->string) -> ('c -> string) -> string
