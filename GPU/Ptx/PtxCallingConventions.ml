(* pp: -parser o pa_macro.cmo *)

type data_location = 
  | ScalarInput
  (* offset into constants table for shape *)
  | GlobalInput of GpuVal.data_layout * int 
  | GlobalOutput of int  (* offset into constant buffer *) 
  (* name of the texture, geometry type, and constants offset for shape *)   
  | TextureInput of string * Ptx.geom * GpuVal.data_layout * int  
  | ConstantInput of int * int (* offset into constant buffer *) 

(* map Imp identifiers to their realization within a Ptx kernel *)
type calling_conventions = {
  data_locations : data_location ID.Map.t;
  (* what's the order values are passed? *)
  param_order : ID.t array; 
  
  
} 

let loc_to_str = function 
  | ScalarInput -> "scalar"
  | GlobalInput _ -> "global input"
  | GlobalOutput _ -> "global output"
  | TextureInput (str, geom,_, _) -> 
      Printf.sprintf "%s texture: %s" (Ptx.ptx_geom_to_str geom) str
  | ConstantInput (_, offset) -> Printf.sprintf "constant[%d]" offset


