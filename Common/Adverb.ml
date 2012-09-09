type adverb_type = Map | Reduce | Scan 

let adverb_type_to_str = function 
  | Map -> "map"
  | Reduce -> "reduce"
  | Scan -> "scan"

type 'a  init = 
  | InitValues of 'a 
  | InitFirstElt


type ('a, 'b, 'c) t = { 
  adverb_type : adverb_type; 
  fn : 'a; 
  fixed : 'b;
  args : 'b; 
  axes : 'c; 
  (* initialize scans and reductions either with value or function mapping
     array element to acc 
  *) 
  init : ('b init) option; 
  (* given two accumulators, combine them into a single new accumulator
     if this function isn't given we can't parallelize reductions & scans 
  *) 
  combine : 'a option;  
}

let apply_to_init values = function 
  | InitValues vs -> InitValues (values vs)
  | InitFirstElt -> InitFirstElt 
  

let apply_to_fields adverb ~(fn:'a -> 'd) ~(values:'b -> 'e) ~(axes:'c -> 'f) =
  { adverb with  
      fn = fn adverb.fn;
      fixed = values adverb.fixed; 
      args = values adverb.args; 
      axes = axes adverb.axes; 
      init = Option.map (apply_to_init values) adverb.init; 
      combine = Option.map fn adverb.combine; 
  }  

let init_to_str values_to_str = function
  (*| InitFn f -> fn_to_str f*) 
  | InitValues vs ->  values_to_str vs 
  | InitFirstElt -> "first-elt" 

let to_str adverb fn_to_str values_to_str axes_to_str =
  Printf.sprintf 
    "%s[fn=%s; combine_fn=%s; init=%s; axes=%s; fixed=(%s)](%s)"
    (adverb_type_to_str adverb.adverb_type)
    (fn_to_str adverb.fn)
    (Option.map_default fn_to_str "none" adverb.combine) 
    (Option.map_default 
      (init_to_str values_to_str) "none" adverb.init)
    (axes_to_str adverb.axes)
    (values_to_str adverb.fixed)
    (values_to_str adverb.args)
