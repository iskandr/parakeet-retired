open Base
open SSA
open SSA_Transform 

let alloc_analysis initSet = object 
    inherit default_transformation 
    
    val set : ID.t MutableSet.t = MutableSet.create 127
    
    method add_to_set ids types = 
      List.iter2 
        (fun id t -> if not $ DynType.is_scalar t then MutableSet.add  set id)
        ids
        types  
       
    method before_fundef fundef =
      add_to_set fundef.input_ids (DynType.fn_input_types fundef.fn_type)
      add_to_set fundef.output_ids ( DynType.fn_output_types fundef.fn_type);
      NoChange   
       
    method stmt env stmtNode = match stmtNode.stmt with
      | Set (_, {exp=ArrayIndex _}) -> NoChange   
      | Set(ids, rhs) ->
          (* if rhs wasn't an array slicing expression, 
             add every array created to the set of objects
             which must be allocated
          *) 
          add_to_set ids rhs.exp_types; NoChange 
      | _ -> NoChange 
end

(* for now this just returns the set of vectors which weren't 
   created by a slicing operation. In the future we should 
   figure out whether slices are copied when written to-- but
   for now we assume all arrays are read-only. 
*) 

let rec infer_fundef fundef =
  (* any input/output arrays must be allocated *)
  in 
  let finalSet, _ = 
    SSA_Base_Analysis.eval_block alloc_logic initSet fundef.body
  in 
  finalSet 
      
      