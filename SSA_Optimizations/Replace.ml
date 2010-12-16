(* pp: -parser o pa_macro.cmo *)

open Base
open SSA 
open SSA_Transform 

module type REPLACE_PARAMS = sig
  val idMap : ID.t ID.Map.t  
end 
module Replace_Rules(P: REPLACE_PARAMS) = struct
  include P 
  type context = unit 
  let init _ = () 
  let dir = Forward 
  
  let rec replace_id_list = function 
  | [] -> [], false
  | id::ids -> 
      let ids', changed = replace_id_list ids in 
      if ID.Map.mem id idMap then (ID.Map.find id idMap)::ids', true
      else id::ids', changed 
      
  let value _ valNode = match valNode.value with  
    | Var id -> 
      if ID.Map.mem id idMap then 
        Update { valNode with value= Var (ID.Map.find id idMap) }
      else NoChange 
    | _ -> NoChange
 
  let exp _ _ = NoChange
   
  let stmt _ stmtNode = match stmtNode.stmt with 
    | Set (ids, rhs) ->
        let ids', changed = replace_id_list ids in 
        if changed then 
          Some [SSA.mk_set ?src:stmtNode.stmt_src ids' rhs]
        else None 
    | SetIdx (id, indices, rhs) ->
        if ID.Map. mem id idMap then 
          let id' = ID.Map.find id idMap in 
          Some [SSA.mk_stmt ?src:stmtNode.stmt_src $ SetIdx(id', indices, rhs)]
        else None  
    | If (c, t, f, gate) ->
      let outIds, outChanged  = replace_id_list gate.if_output_ids in
      let trueIds, trueChanged = replace_id_list gate.true_ids in
      let falseIds, falseChanged = replace_id_list gate.false_ids in
      if outChanged || trueChanged || falseChanged then 
        let gate' = { 
          if_output_ids = outIds; true_ids = trueIds; false_ids = falseIds
        } in Some [SSA.mk_if ?src:stmtNode.stmt_src c t f gate'] 
      else None   
    | _ -> None 
end

      
let replace_block idMap block =  
  let module Params = struct let idMap = idMap end in 
  let module Replacer = MkTransformation(Replace_Rules(Params)) in  
  Replacer.transform_block () block  
     
let replace_fundef idMap fundef = 
  let body', changed = replace_block idMap fundef.body in 
  {fundef with body = body'}, changed   