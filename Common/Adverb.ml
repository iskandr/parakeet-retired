type t = Map | Reduce | Scan 


type ('a, 'b) init = 
  | IniFn of 'a 
  | InitValues of 'b 

type ('a, 'b, 'c) adverb_info { 
  adverb : t; 
  adverb_fn : 'a; 
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

let apply_to_init fn values = function 
  | InitFn f -> InitFn (fn f)
  | InitValues vs -> InitValues (values vs)

let apply_to_fields info ~(fn:'a -> 'd) ~(values:'b -> 'e) ~(axes:'c -> 'f) =
  { info with  
      fn = fn adverb.fn;
      fixed_args = values adverb.fixed_args; 
      array_args = values adverb.array_args; 
      axes = axes adverb.axes; 
      init = Option.map (apply_to_init fn values) adverb.init; 
      combine_fn = Option.map fn adverb.combine_fn; 
  }  

let init_to_str values_to_str = function

  | InitFirstElement -> "first-element"
  | InitWithValues vs ->  values_to_str vs 
 

let info_to_str info fn_to_str values_to_str axes_to_str =
  Printf.sprintf "%s[fn=%s; combine_fn=%s; fixed=(%s); init=%s; axes=%s](%s)"
    (to_str info.adverb)
    (fn_to_str info.adverb_fn)
    (Option.default "none" (Option.map fn_to_str info.combine_fn))
    (values_to_str info.fixed_args)
    (init_to_str values_to_str info.init)
    (axes_to_str info.axes)
    (values_to_str info.array_args)
