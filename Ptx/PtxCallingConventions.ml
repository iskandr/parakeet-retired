(* pp: -parser o pa_macro.cmo *)

type data_location = 
  | ScalarInput
  | GlobalInput
  | TextureInput of string * Ptx.geom
  | ConstantInput of int (* offset into constant buffer *) 

(* map Imp identifiers to their realization within a Ptx kernel *)
type calling_conventions = {
  data_locations : data_location ID.Map.t;
  (* what's the order values are passed? *)
  param_order : ID.t array
} 

let loc_to_str = function 
  | ScalarInput -> "scalar"
  | GlobalInput -> "global"
  | TextureInput (str, geom) -> 
      Printf.sprintf "%s texture: %s" (Ptx.ptx_geom_to_str geom) str
  | ConstantInput offset -> Printf.sprintf "constant[%d]" offset


