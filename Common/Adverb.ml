type t = Map | Reduce | Scan | AllPairs


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

let apply_to_map_fields info fn values axes =
  { 
    map_fn = fn info.map_adverb_fn;
    map_fixed_args = values info.map_fixed_args; 
    map_array_args = values info.map_array_args; 
    map_axes = axes info.map_axes; 
  }
  
let apply_to_reduce_fields info fn values axes = 
  { 
    reduce_fn = fn info.reduce_fn; 
    init = (match info.init with 
      | InitValues vs -> InitWithValues (values vs)
      | InitFirstElement -> InitFirstElement)


    reduce_fixed_args = values info.reduce_fixed_args; 
    combiner = fn info.combiner; 
    
    
  reduce_fixed_args : 'b;
  init : 'b init; 
  reduce_array_args : 'b; 
  reduce_axes : 'c;
  keep_prefix : bool; 
    combine_fn = fn info.combine_fn; 
    fixed_args = values info.fixed_args;
    init = (match info.init with 
      | InitWithValues vs -> InitWithValues (values vs)
      | other -> other)
    axes = axes info.axes;
    array_args = values info.array_args; 

let apply_to_fields info ~(fn:'a -> 'd) ~(values:'b -> 'e) ~(axes:'c -> 'f) =
  match info with 
    | Map map_info -> apply_to_map_fields map_info fn values axes 
    | Reduce reduce_info -> apply_to_reduce_fields reduce_info fn values axes
    | Scan scan_info -> apply_to_scan_fields scan_info fn values axes 

 {
    adverb = info.adverb;

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
