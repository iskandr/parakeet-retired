open Base
open SSA 


let rec replace_id_list idMap = function 
  | [] -> []
  | id::ids -> (PMap.find_default id idMap id) :: (replace_id_list idMap ids)

let rec replace_block idMap = function 
  | [] -> [] 
  | stmtNode::rest -> (replace_stmt idMap stmtNode)::(replace_block idMap rest)

and replace_value idMap vNode = match vNode.value with  
  | Var id -> let id' = PMap.find_default id idMap id in 
    { vNode with value=Var id'} 
 
  | Lam fundef -> 
    { vNode with value = 
       Lam {fundef with body = replace_block idMap fundef.body }
    } 
  | simple -> vNode 
and replace_exp idMap expNode = 
  let exp' = match expNode.exp with  
  | Arr elts ->  Arr  (List.map (replace_value idMap) elts)
  
  | Values vs -> Values ( List.map (replace_value idMap) vs)    
  
  | Arr elts -> Arr  (List.map (replace_value idMap) elts)
  
  | App (fn, args) -> 
      let fn' = replace_value idMap fn in 
      let args' = List.map (replace_value idMap) args in 
      App (fn', args') 
    
  | ArrayIndex (lhs, args) -> 
      let lhs' = replace_value idMap lhs in 
      let args' = List.map (replace_value idMap) args in 
      ArrayIndex (lhs', args')
  in {expNode with exp = exp' } 
      
and replace_stmt idMap stmtNode = 
  match stmtNode.stmt with  
  | Set (ids, exp) ->
      let exp' = replace_exp idMap exp in
      let ids' = 
        List.map 
          (fun id -> if PMap.mem id idMap then PMap.find id idMap else id)
          ids
      in 
      { stmtNode with stmt = Set(ids', exp')}
  | Ignore exp -> {stmtNode with stmt = Ignore (replace_exp idMap exp) } 
  | SetIdx (id, indices, rhs) -> 
      let indices' = List.map (replace_value idMap) indices in 
      let rhs' = replace_value idMap rhs in
      if PMap.mem id idMap then 
        { stmtNode with stmt = SetIdx(PMap.find id idMap, indices', rhs')}
      else stmtNode 
       
  | If (cond, tBlock, fBlock, gate) -> 
      let cond' = replace_value idMap cond in
      let tBlock' = replace_block idMap tBlock in
      let fBlock' = replace_block idMap fBlock in
      let gate' = {
        if_output_ids = replace_id_list idMap gate.if_output_ids; 
        true_ids = replace_id_list idMap gate.true_ids; 
        false_ids = replace_id_list idMap gate.false_ids; 
      } in 
      {stmtNode with stmt =  If (cond', tBlock', fBlock', gate')}   
      
