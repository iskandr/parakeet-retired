open Base
open SSA 
open SSA_Transform 

(* doesn't update the type environment, which still refers to the old *)
(* identifiers! *) 

module type REPLACE_PARAMS = sig
  val idMap : ID.t ID.Map.t  
end 
module Replace_Rules(P: REPLACE_PARAMS) = struct
  
  let rec replace_id_list idMap ids = match ids with  
  | [] -> []
  | id::rest -> 
      let rest' = replace_id_list idMap rest in 
      if ID.Map.mem id idMap then (ID.Map.find id idMap)::rest'
      (* use memory equality to check if anything has changed *) 
      else if rest == rest' then ids else id::rest' 
   
  type context = ID.t ID.Map.t   
  let init _ = P.idMap  
  let finalize idMap fundef =
    Update {fundef with 
      input_ids = replace_id_list idMap fundef.input_ids;
      output_ids = replace_id_list idMap fundef.output_ids; 
    }    
      
  let dir = Forward 
  
      
  let value idMap valNode = match valNode.value with  
    | Var id -> 
      (*Printf.printf "~~~ Replace: %d => %d \n" id (ID.Map.find id idMap);*) 
      
      if ID.Map.mem id idMap then 
        Update { valNode with value= Var (ID.Map.find id idMap) }
      else NoChange 
    | _ -> NoChange
 
  let exp _ _ = NoChange
   
  let stmt idMap stmtNode = match stmtNode.stmt with 
    | Set (ids, rhs) ->
        let ids' = replace_id_list idMap ids in
        (* if any changes made, then memory location of ids and ids' *)
        (* will be different *)  
        if ids != ids' then Update (SSA.mk_set ?src:stmtNode.stmt_src ids' rhs)
        else NoChange 
    | SetIdx (id, idxs, rhs) ->
        if ID.Map. mem id idMap then 
          let id' = ID.Map.find id idMap in 
          Update (SSA.mk_stmt ?src:stmtNode.stmt_src (SetIdx(id', idxs, rhs)))
        else NoChange
    | _ -> NoChange
end

let replace_fundef idMap fundef = 
  let module Params = struct let idMap = idMap end in 
  let module Replacer = MkSimpleTransform(Replace_Rules(Params)) in  
  Replacer.transform_fundef fundef 