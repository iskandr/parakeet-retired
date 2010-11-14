open Base
open AST
open AST_Info
open SSA


(* environment mapping strings to SSA IDs or global function IDs *)
module Env = struct 

  (* Assume all functions have been moved to global scope via lambda lifting. 
     For now don't support data at global scope.
     Also, the global scope is described by an abstract function 
     from names to FnId's so we can easily pass in accessors to 
     either Hashtbl's or ID.Map.t. 
  *) 
  type t = 
  | GlobalScope of (string -> FnId.t) 
  | LocalScope of (ID.t String.Map.t) * t
  

  let add_data_binding env name id = match env with 
  | GlobalScope _ -> failwith "can't add data binding at global scope"
  | LocalScope (dataMap, parent) -> 
      LocalScope (String.Map.add name id dataMap, parent)

  let rec add_data_bindings env names ids = match names, ids with 
    | [], _
    | _, [] -> env 
    | name::moreNames, id::moreIds -> 
        add_data_bindings (add_data_binding env name id) moreNames moreIds
            
  let extend (parentEnv : t) (names : string list) (ids : ID.t list) : t =
    let localMap = String.Map.extend String.Map.empty names ids in 
    LocalScope(localMap, parentEnv)  

  (* 
     Recursively lookup variable name in nested scopes, return an 
     SSA value-- either Var _ for data or Fn _ for a function   
  *) 
  let rec lookup_ssa env name = match env with 
  | GlobalScope fnLookup ->  SSA.GlobalFn (fnLookup name)
  | LocalScope (dataEnv, parent) -> 
      if String.Map.mem name dataEnv then 
        SSA.Var (String.Map.find name dataEnv)
      else lookup_ssa parent name

  (* assume that we know this name is not of a global function *) 
  let lookup_ssa_id env name  = 
    match lookup_ssa env name with 
    | SSA.Var id -> id 
    | _ -> assert false 
end 

open Env 

        
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
          let trueBlock, (trueEnv : Env.t) = 
            translate_stmt condEnv ~value_id:trueRetId trueNode 
          in
          let trueIds = List.map (lookup_ssa_id trueEnv) mergeNames in
          let falseRetId = ID.gen() in  
          let falseBlock, falseEnv = 
            translate_stmt condEnv ~value_id:falseRetId falseNode in
          let falseIds = List.map (lookup_ssa_id falseEnv) mergeNames in
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
            true_ids = List.map (lookup_ssa_id trueEnv) mergeNames;
            false_ids =List.map (lookup_ssa_id falseEnv) mergeNames;
          } in 
          trueBlock, falseBlock, ifGate
      in    
      let stmt = mk_stmt $ If(condVal,trueBlock,falseBlock, gate) in
      let env' = add_data_bindings env mergeNames mergeIds in 
      condStmts @ [stmt], env' 
   
  | AST.Def(name, rhs) -> 
      let id = ID.gen () in 
      let rhsStmts, rhsEnv, rhsExp = translate_exp env rhs in 
      (* is a return value expected in value_id? *)
      let stmts = match value_id with 
        | Some valId -> 
          [mk_set [id] rhsExp;
           mk_set [valId] $ mk_val_exp (Var id)
          ]
        | None ->  [mk_stmt $ Set([id], rhsExp)]
      in
      let env' = add_data_binding rhsEnv name id in 
      rhsStmts @ stmts, env' 
       
  | AST.Block [] -> [], env         
  | AST.Block [node] -> translate_stmt env ?value_id node
  | AST.Block (node::nodes) ->
      let nodeStmts, nodeEnv = translate_stmt env node in
      let restNode = {node with AST.data = AST.Block nodes } in 
      let restStmts, restEnv = 
        translate_stmt nodeEnv ?value_id restNode in 
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
  let mk_exp e = 
    { exp= e; exp_src=Some node.src; exp_types=[DynType.BottomT]} 
  in
  (* simple values generate no statements and don't modify the env *) 
  let value v =
    let expNode = 
      {
        exp = Values [mk_val v]; 
        exp_src=Some node.src; 
        exp_types=[DynType.BottomT]
      } 
    in 
    [], env, expNode 
     
  in  
  match node.data with 
  | AST.Var name -> value $ lookup_ssa env name 
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
      let fundef = translate_fn env vars body in 
      value (Lam fundef) 
       
  | AST.Arr args -> 
      let stmts, ssaEnv, ssaArgs = translate_args env args in 
      stmts, ssaEnv, SSA.mk_arr ssaArgs
  | AST.If _  -> failwith "unexpected If while converting to SSA"
  | AST.Def _ -> failwith "unexpected Def while converting to SSA"
  | AST.SetIdx _ -> failwith "unexpected SetIdx while converting to SSA"
  | AST.Block _  -> failwith "unexpected Block while converting to SSA"
  | AST.WhileLoop _ -> failwith "unexpected WhileLoop while converting to SSA"
  | AST.CountLoop _ -> failwith "unexpected CountLoop while converting to SSA"
   
                    
and translate_value env node =
  (* simple values generate no statements and don't modify the environment *)  
  let return v = [], env, mk_val v in 
  match node.AST.data with 
  | AST.Var name -> return $ lookup_ssa env name 
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
and translate_fn parentEnv argNames body = 
  let retId = ID.gen () in
  let nargs = List.length argNames in 
  let argIds = ID.gen_fresh_list nargs in
  (* map string names to their SSA identifiers -- 
     assume globalMap contains only functions 
  *)  
  let initEnv = extend parentEnv argNames argIds in  
  let stmts, _ = translate_stmt initEnv ~value_id:retId body in
  (* make an empty type env since this function hasn't been typed yet *) 
  SSA.mk_fundef 
    ~body:stmts 
    ~tenv:ID.Map.empty 
    ~input_ids:argIds 
    ~output_ids:[retId]  