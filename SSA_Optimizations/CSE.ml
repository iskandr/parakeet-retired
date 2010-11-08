open DynType 
open Base
open SSA 

(* expressions without side effects *) 
let is_safe_exp expNode = match expNode.exp with  
  | App({value=Prim _}, _) 
  | Arr _  
  | Values _ -> true
  (* assume function calls unsafe by default-- might be too strong*)
  | _ -> false 

let rec eval_block env = function 
  | [] -> [], false
  | stmtNode::rest -> 
      let stmtNode', env', stmtChanged = eval_stmt env stmtNode in
      let rest', restChanged = eval_block env' rest in 
      stmtNode'::rest', stmtChanged || restChanged 
  
and eval_stmt (env: (exp,value) PMap.t) (stmtNode : stmt_node)  =
  let stmt', env', changed = match stmtNode.stmt with 
  | Set ([id], expNode) when is_safe_exp expNode -> 
    if PMap.mem expNode.exp env then ( 
      let rhsVal = PMap.find expNode.exp env  in
      let rhs' = {
        value=rhsVal; 
        value_type=List.hd expNode.exp_types; 
        value_src=None
      } in 
      let expNode' = {expNode with exp = Values [rhs'] } in
      Set([id], expNode'), env, true
    ) 
    else
      let expNode', changed = eval_exp env expNode in    
      let env' = PMap.add expNode'.exp (Var id) env in 
      Set ([id], expNode'), env', changed
  (* if not a safe expression  *) 
  | Set (ids,rhs) ->  Set(ids,rhs), env, false
  | Ignore exp -> Ignore exp, env, false 
    
  | If (cond, tBlock, fBlock, gate) -> 
    (* for now don't try to eliminate expressions common between branches, 
       and ignore the subexpressions computed in each branch since 
       those values won't be unconditionally available 
    *)
    let tBlock', tChanged = eval_block env tBlock in
    let fBlock', fChanged = eval_block env fBlock in 
    If(cond, tBlock', fBlock', gate), env, tChanged || fChanged
  in 
  {stmtNode with stmt = stmt'}, env', changed 
and eval_exp (env : (exp,value) PMap.t) expNode =
  let exp', changed = match expNode.exp with 
  | Values [v] -> 
      let v', changed = eval_value env v in 
      Values [v'], changed  
  | Arr vs -> 
      let vs', changed = eval_value_list env vs in 
      Arr vs', changed
  (*| Tuple vs -> 
      let vs', changed = eval_value_list env vs in 
      Tuple vs', changed
  *)
  | App(fn, args) -> 
      let fn', fnChanged = eval_value env fn in 
      let args', argsChanged = eval_value_list env args in 
      App(fn', args'), fnChanged || argsChanged   
  | exp -> exp, false   
  in 
  {expNode with exp =exp'}, changed 
and eval_value env valNode = 
  let value', changed = match valNode.value with  
  | Lam fundef -> 
    let body',changed = eval_block env fundef.body in 
    Lam {fundef with body = body'}, changed
  | v -> v, false 
  in {valNode with value = value'}, changed 

and eval_value_list env = function 
  | [] -> [], false
  | v::vs -> 
      let v', changed = eval_value env v in 
      let rest, restChanged = eval_value_list env vs in 
      v'::rest, changed || restChanged 
      
let cse (fnTable : FnTable.t) fundef = 
  let body', changed = eval_block PMap.empty fundef.body in 
  { fundef with  body = body'}, changed 
      