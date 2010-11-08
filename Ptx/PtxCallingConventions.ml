

type data_location = 
  | ScalarInput 
  | GlobalInput 
  | TextureInput of string * Ptx.geom 

(* map Imp identifiers to their realization within a Ptx kernel *) 
type calling_conventions = {
  data_locations : data_location ID.Map.t;
  (* what's the order values are passed? *) 
  param_order : ID.t array
} 
    