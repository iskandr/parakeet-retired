open Base
open SSA
open FindDefs


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
  | App ({value=Var id} as fnNode, args) -> 
    (match ID.Map.find id defEnv with 
     (* assume that function was partially applied and thus returns 
        just a single value -- the closure 
     *)
     | SingleDef (App({value=GlobalFn fnId}, closureArgs), 1, 1) ->
       {expNode with exp = 
            App({fnNode with value = GlobalFn fnId}, closureArgs @ args)
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
        
let elim_partial_apps fnTable fundef =
  let defEnv = FindDefs.find_function_defs fundef in
  let block = fundef.SSA.body in  
  let constEnv = FindConstants.find_constants block in
  let body', changed  = 
    eval_block defEnv constEnv block
  in 
  {  fundef with body = body'}, changed 