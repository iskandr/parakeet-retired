open Base
open SSA

let add_to_set s ids types = 
  List.iter2 
    (fun id t -> if not $ DynType.is_scalar t then MutableSet.add s id)
    ids
    types  

module Env = struct
  type env = ID.t MutableSet.t
  let init fundef =
    let set = MutableSet.create 127 in 
    add_to_set set fundef.input_ids (DynType.fn_input_types fundef.fn_type);
    add_to_set set fundef.output_ids ( DynType.fn_output_types fundef.fn_type);
    set
end 

module AllocAnalysis = struct 
  include SSA_Analysis.MakeSimpleAnalysis(Env) 

  let set env ids rhs _ = match rhs.exp with 
    | ArrayIndex _ -> 
        SSA_Analysis.NoChange (* I don't even remember why this works *)  
    | _ -> 
      add_to_set env ids rhs.exp_types;
      SSA_Analysis.NoChange    
end

(* for now this just returns the set of vectors which weren't 
   created by a slicing operation. In the future we should 
   figure out whether slices are copied when written to-- but
   for now we assume all arrays are read-only. 
*) 

let infer_fundef = SSA_Analysis.MakeAnalyzer(AllocAnalysis).analyze_fundef