open Base
open SSA
open UntypedFindDefs
open UntypedFindConstants 


let rec eval_block defEnv constEnv block =  
  let rec aux  = function 
    | stmt::stmts -> 
        let stmt', changed = eval_stmt defEnv constEnv stmt in
        let rest, restChanged = aux stmts in 
        stmt' :: rest, (changed || restChanged)  
    | [] -> [], false
  in aux block 
  
and eval_stmt defEnv constEnv stmtNode = 
  match stmtNode.stmt with 
  | Set (ids, rhs) ->  
    let rhs', changed = eval_exp defEnv constEnv rhs in
    {stmtNode with stmt = Set (ids, rhs') }, changed
  | Ignore rhs -> 
    let rhs', changed = eval_exp defEnv constEnv rhs in 
    {stmtNode with stmt = Ignore rhs' }, changed
  | If (cond, trueBlock, falseBlock, ifGate) ->
    let trueBlock', trueChanged = eval_block defEnv constEnv trueBlock in 
    let falseBlock', falseChanged = eval_block defEnv constEnv falseBlock in
    let stmtNode' = 
      {stmtNode with stmt = If(cond, trueBlock', falseBlock', ifGate)}
    in 
    stmtNode', trueChanged || falseChanged     
  | other -> stmtNode, false
and eval_exp defEnv constEnv expNode = 
  match expNode.exp with 
  | App ({value=Var fnId} as fnNode, args) -> 
    (match PMap.find fnId defEnv with 
     | SingleDef (App({value=Var fnId'}, args')) 
       when is_function_constant constEnv fnId' ->
       {expNode with exp = 
            App({fnNode with value = Var fnId'}, args' @ args)
       }, true
     | _ -> expNode, false
    )
  | Values vs -> 
    let vs', changed = eval_value_list defEnv constEnv vs in 
    {expNode with exp = Values vs'}, changed
  
  | _ -> expNode, false  
and eval_value defEnv constEnv vNode = 
  let value', changed = match vNode.value with  
  | Lam fundef -> 
    let body', changed = eval_block defEnv constEnv fundef.body in 
    Lam {fundef with body = body'}, changed
  | v -> v, false 
  in {vNode with value = value'}, changed 
and eval_value_list defEnv constEnv = function 
  | [] -> [], false
  | v::vs -> 
    let v', currChanged = eval_value defEnv constEnv v in
    let vs', restChanged = eval_value_list defEnv constEnv vs in 
    v'::vs', currChanged || restChanged
        
let elim_partial_apps fnTable block = 
  let defEnv = UntypedFindDefs.find_defs block in
  let constEnv = UntypedFindConstants.find_constants block in 
  eval_block defEnv constEnv block 