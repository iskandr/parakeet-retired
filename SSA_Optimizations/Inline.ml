(* pp: -parser o pa_macro.cmo *)

open Base
open SSA 
open DynType 
open SSA_Transform 

let do_inline fundef argVals = 
  let idSet : ID.Set.t = FindBindingSet.fundef_bindings fundef in 
  let replaceMap = 
    ID.Set.fold 
      (fun  id accMap -> let id' = ID.gen() in ID.Map.add id id' accMap)
      idSet 
      ID.Map.empty
  in   
  let freshFundef, _ = Replace.replace_fundef replaceMap fundef in 
  let argAssignments = 
    mk_set 
      freshFundef.input_ids 
      (SSA.mk_exp ~types:freshFundef.fundef_input_types (Values argVals)) 
  in
  let outputValNodes = 
    List.map2 
      (fun id t -> SSA.mk_var ~ty:t id) 
      freshFundef.output_ids 
      freshFundef.fundef_output_types 
  in 
  let outputExp = 
    mk_exp ~types:freshFundef.fundef_output_types (Values outputValNodes) 
  in
  (* list of new ids and their types-- ignore types missing from tenv *) 
  let typesList : (ID.t * DynType.t) list = 
    ID.Set.fold  
      (fun oldId accList ->
          if ID.Map.mem oldId fundef.tenv then
            let newId = ID.Map.find oldId replaceMap in 
            (newId, ID.Map.find oldId fundef.tenv)::accList
          else accList
      )
      idSet
      [] 
  in 
  let body' = block_append (block_of_stmt argAssignments) freshFundef.body in  
  body', outputExp, typesList 
  
module type INLINE_PARAMS = sig 
  val lookup : FnId.t -> fundef option 
end 

module Inline_Rules (P:INLINE_PARAMS) = struct
  include P 
   
  type context = (DynType.t ID.Map.t) ref
  let init fundef = ref fundef.tenv   
  let finalize _ _ = NoChange 
  let dir = Forward 
  
  let rec add_types_list envRef = function 
    | [] -> envRef
    | (id,t)::rest -> 
        envRef := ID.Map.add id t !envRef;  
        add_types_list envRef rest 
        
         
  let stmt envRef stmtNode = NoChange     
  let exp envRef expNode = match expNode.exp with 
    | App ({value=GlobalFn fnId} as fn, args) -> 
      (match P.lookup fnId with 
        | None -> NoChange  
        | Some fundef -> 
          (* make sure arity lines up *)
          if List.length fundef.input_ids <> List.length args then NoChange
          else 
          let inlineBlock, outputExp, typesList = do_inline fundef args in
          add_types_list envRef typesList;
          IFDEF DEBUG THEN 
            assert (outputExp.exp_types = expNode.exp_types);
          ENDIF;  
          let expNode' = {outputExp with exp_src=expNode.exp_src } in
          UpdateWithBlock(expNode', inlineBlock)
       )
    | _ -> NoChange 
  
  let value env valNode = NoChange       
end 


let run_fundef_inliner (functions : FnTable.t) fundef =
  let module Params = 
    struct let lookup id  = FnTable.find_option id functions end
  in  
  let module Inliner = SSA_Transform.MkSimpleTransform(Inline_Rules(Params)) in
  let fundef', changed = Inliner.transform_fundef fundef in 
  let tenv' = !(Inliner.get_context ()) in
  {fundef' with tenv = tenv' }, changed    