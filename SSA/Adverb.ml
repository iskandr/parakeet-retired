

type t = Map | Reduce | Scan | AllPairs

let to_str = function
  | Map -> "map"
  | Reduce -> "reduce"
  | Scan -> "scan"
  | AllPairs -> "allpairs"

type ('a,'b,'c) info = {
  adverb : t;
  adverb_fn : 'a;
  fixed_args : 'b;
  init : 'b option;
  axes : 'c;
}

let apply_to_fields (f:'a->'d) (g:'b -> 'e) (h:'c -> 'f) info = {
  adverb = info.adverb;
  adverb_fn = f info.adverb_fn;
  fixed_args = g info.fixed_args;
  init = Option.map g info.init;
  axes = h info.axes;
}

let info_to_str fn_to_str args_to_str axes_to_str info =
  Printf.sprintf "%s[fn=%s; fixed=%s; init=%s; axes=%s]"
    (to_str info.adverb)
    (fn_to_str info.adverb_fn)
    (args_to_str info.fixed_args)
    (match info.init with None -> "None" | Some init -> args_to_str init)
    (axes_to_str info.axes)