

type t = Map | Reduce | Scan | AllPairs

let to_str = function
  | Map -> "map"
  | Reduce -> "reduce"
  | Scan -> "scan"
  | AllPairs -> "allpairs"

let has_accumulator = function
  | Scan | Reduce -> true
  | _ -> false

type ('a,'b,'c) info = {
  adverb : t;
  adverb_fn : 'a;
  fixed_args : 'b;
  init : 'b option;
  axes : 'c;
  array_args : 'b; 
}


let apply_to_fields info ~(fn:'a->'d) ~(values:'b -> 'e) ~(axes:'c -> 'f) =
  {
    adverb = info.adverb;
    adverb_fn = fn info.adverb_fn;
    fixed_args = values info.fixed_args;
    init = Option.map values info.init;
    axes = axes info.axes;
    array_args = values info.array_args; 
  }

let info_to_str info fn_to_str values_to_str axes_to_str =
  Printf.sprintf "%s[fn=%s; fixed=%s; init=%s; axes=%s](%s)"
    (to_str info.adverb)
    (fn_to_str info.adverb_fn)
    (values_to_str info.fixed_args)
    (match info.init with None -> "None" | Some init -> values_to_str init)
    (axes_to_str info.axes)
    (values_to_str info.array_args)
