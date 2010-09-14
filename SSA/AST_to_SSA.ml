open Base
open AST
open AST_Info
open SSA


(* try to find SSA binding for name in the current scope, the surrounding scope
   or just default to that variable being undefined 
*) 
let lookup_ssa_id  parentEnv currEnv name  = 
  if PMap.mem name currEnv then PMap.find name currEnv 
  else if PMap.mem name parentEnv then PMap.find name parentEnv 
  else ID.undefined 

(* retId is an optional parameter, if it's provided then the generated 
   statements must set retId to the last value 
*)
let rec translate_stmt env ?value_id node =  
  let mk_stmt s = SSA.mk_stmt ~src:node.AST.src s in   
  match node.AST.data with
  | AST.If(cond, trueNode, falseNode) ->
      let condStmts, condEnv, condVal = translate_value env cond in
     (* let tNames = PSet.inter trueNode.ast_info.defs trueNode.info.used_after in*) 
      (*let fNames = PSet.inter trueNode.defs trueNode.info.used_after in*)
      let tNames = trueNode.ast_info.defs_local in 
      let fNames = falseNode.ast_info.defs_local in 
      let mergeNames = PSet.to_list $ PSet.union tNames fNames in  
      let mergeIds = ID.gen_fresh_list (List.length mergeNames) in
      
      (* is this If an expression or an effectful statement? *)
      let trueBlock, falseBlock, gate = match value_id with 
      | Some valId -> 
          let trueRetId = ID.gen () in 
          let trueBlock, trueEnv = 
            translate_stmt condEnv ~value_id:trueRetId trueNode in
          let trueIds = List.map (lookup_ssa_id env trueEnv) mergeNames in
          let falseRetId = ID.gen() in  
          let falseBlock, falseEnv = 
            translate_stmt condEnv ~value_id:falseRetId falseNode in
          let falseIds = List.map (lookup_ssa_id env falseEnv) mergeNames in
          let ifGate = { 
            if_output_ids = valId :: mergeIds;
            true_ids = trueRetId :: trueIds;
            false_ids = falseRetId :: falseIds;
          } in 
          trueBlock, falseBlock, ifGate
          
      | None ->
          (* note that here we're not passing any return/value IDs *) 
          let trueBlock, trueEnv = translate_stmt condEnv trueNode in
          let falseBlock, falseEnv = translate_stmt condEnv falseNode in
          let ifGate = { 
            if_output_ids = mergeIds;
            true_ids = List.map (lookup_ssa_id env trueEnv) mergeNames;
            false_ids =List.map (lookup_ssa_id env falseEnv) mergeNames;
          } in 
          trueBlock, falseBlock, ifGate
      in    
      let stmt = mk_stmt $ If(condVal,trueBlock,falseBlock, gate) in
      let env' = 
        List.fold_left2 (fun env name id -> PMap.add name id env) 
          PMap.empty mergeNames mergeIds
      in  
      condStmts @ [stmt], env' 
   
  | AST.Def(name, rhs) -> 
      let id = ID.gen () in 
      let rhsStmts, rhsEnv, rhsExp = translate_exp env rhs in 
      let env' = PMap.add name id rhsEnv in
      (* is a return value expected in value_id? *)
      let stmts = match value_id with 
        | Some valId -> 
          [mk_set [id] rhsExp;
           mk_set [valId] $ mk_val_exp (Var id)
          ]
        | None ->  [mk_stmt $ Set([id], rhsExp)]
      in 
      rhsStmts @ stmts, env' 
       
  | AST.Block [] -> [], env         
  | AST.Block [node] -> translate_stmt env ?value_id node
  | AST.Block (node::nodes) ->
      let nodeStmts, nodeEnv = translate_stmt env node in
      let restNode = {node with AST.data = AST.Block nodes } in 
      let restStmts, restEnv = translate_stmt nodeEnv ?value_id restNode in 
      nodeStmts @ restStmts, restEnv   
  
  | AST.SetIdx(name, indices, rhs) -> failwith "setidx not implemented"
  | AST.WhileLoop(cond,code) -> failwith "while loop not implemented"
  | AST.CountLoop(upper,code) -> failwith "count loop not implemented"
  | simple ->  
      let stmts, env', exp = translate_exp env node in
      (* simple values shouldn't contain any statements *) 
      let assign = match value_id with 
        | Some valId -> [mk_stmt $ Set([valId], exp)]
        | None -> [mk_stmt $ Ignore exp]
      in stmts @ assign, env'

and translate_exp env node =
  let mk_exp e = { exp= e; exp_src=Some node.src; exp_types=[]} in
  (* simple values generate no statements and don't modify the env *) 
  let value v = 
    [], env, {exp = Values [mk_val v]; exp_src=Some node.src; exp_types=[]} 
  in  
  match node.data with 
  | AST.Var name -> value (Var (PMap.find name env))
  | AST.Prim p -> value (Prim p)
  | AST.Num n -> value (Num n)
  | AST.Str s -> value (Str s)
  | AST.Sym s -> value (Sym s)
  | AST.Void -> value Unit 
  | AST.App (fn, args) ->
      let fnStmts, fnEnv, fn' = translate_value env fn in 
      let argStmts, argEnv, args' = translate_args fnEnv args in
      let stmts = fnStmts @ argStmts in 
      let app' = mk_exp $ App(fn', args') in
      stmts, argEnv, app'
  | AST.Lam (vars, body) -> 
      let fundef, _ = translate_fn env vars body in 
      value (Lam fundef) 
       
  | AST.Arr nodes -> failwith "[AST->SSA] array not yet implemented" 
  | AST.If (condNode, trueNode, falseNode) -> 
      failwith "[AST->SSA] if not yet implemented" 
  | AST.Def (name, rhs) -> failwith "[AST->SSA] def not yet implemented"
  | AST.SetIdx (var, indices, rhs) -> 
      failwith "[AST->SSA] setidx not yet implemented" 
  | AST.Block nodes -> failwith "[AST->SSA] block" 
  | AST.WhileLoop (condNode, bodyNode) -> failwith "[AST->SSA] while loop"
  | AST.CountLoop _ -> failwith "[AST->SSA] count loop" 
   
                    
and translate_value env node =
  (* simple values generate no statements and don't modify the environment *)  
  let return v = [], env, mk_val v in 
  match node.AST.data with 
  | AST.Var name -> return $ Var (PMap.find name env) 
  | AST.Prim p -> return $ Prim p  
  | AST.Num n -> return $ Num n  
  | AST.Str s -> return $ Str s
  | AST.Sym s -> return $ Sym s
  (* anything not an immediate value must be evaluated in its own statement
     and then named 
  *)
  | _ -> 
      let tempId = ID.gen() in 
      let stmts, env' = translate_stmt env ~value_id:tempId node in 
      stmts, env', mk_val $ Var tempId  
and translate_args env = function 
  | [] -> [] (* no statements *), env (* same env *) , [] (* no values *)   
  | arg::args -> 
      let currStmts, currEnv, v = translate_value env arg in 
      let restStmts, restEnv, vs = translate_args currEnv args in 
      currStmts @ restStmts, restEnv, v :: vs 
(* given the arg names and AST body of function, generate its SSA fundef *)     
and translate_fn env argNames body = 
  let retId = ID.gen () in
  let nargs = List.length argNames in 
  let argIds = ID.gen_fresh_list nargs in
  (* map string names to their SSA identifiers *)  
  let initEnv = 
    List.fold_left2 
      (fun env name id -> PMap.add name id env) 
      env argNames argIds in
  let stmts, finalEnv = translate_stmt initEnv ~value_id:retId body in  
  let ssaFn = { 
    output_ids = [retId]; 
    input_ids = argIds; 
    body = stmts;
    tenv = PMap.empty; 
    fun_type = DynType.BottomT;  
  } in 
  ssaFn, finalEnv 