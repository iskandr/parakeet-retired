open Base
open SSA

let alloc_logic = object 
    inherit [ID.Set.t] SSA_Base_Analysis.base_analysis 
    method stmt env stmtNode = match stmtNode.stmt with
      | Set (_, {exp=ArrayIndex _}) -> env  
      | Set(ids, rhs) ->
          (* if rhs wasn't an array slicing expression, 
             add every array created to the set of objects
             which must be allocated
          *) 
          List.fold_left2 
            (fun accSet id t ->
                if not $ DynType.is_scalar t then ID.Set.add id accSet
                else accSet)
            env
            ids 
            rhs.exp_types
      | _ -> env
end

(* for now this just returns the set of vectors which weren't 
   created by a slicing operation. In the future we should 
   figure out whether slices are copied when written to-- but
   for now we assume all arrays are read-only. 
*) 

let rec infer_fundef fundef =
  (* any input/output arrays must be allocated *)
  let inputTypes = DynType.fn_input_types fundef.fn_type in 
  let outputTypes = DynType.fn_output_types fundef.fn_type in 
  let initSet = 
    List.fold_left2 
      (fun accSet id t -> 
          if not $ DynType.is_scalar t then ID.Set.add id accSet else accSet)
      ID.Set.empty 
      (fundef.input_ids @ fundef.output_ids) 
      (inputTypes @ outputTypes)
  in 
  let finalSet, _ = 
    SSA_Base_Analysis.eval_block alloc_logic initSet fundef.body
  in 
  finalSet 
      
      