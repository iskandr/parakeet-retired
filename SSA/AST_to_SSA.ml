open Base
open AST
open AST_Info
open SSA
open SSA_Codegen 


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

        
(* value_id is an optional parameter, if it's provided then the generated 
   statements must set retId to the last value 
*)
let rec translate_stmt 
          (env:Env.t) 
          (codegen:SSA_Codegen.ssa_codegen) 
          ?value_id 
          (node : AST.node) : Env.t =  
  let mk_stmt s = SSA.mk_stmt ~src:node.AST.src s in
  match node.AST.data with
  | AST.If(cond, trueNode, falseNode) ->
      let condEnv, condVal = translate_value env codegen cond in
      let tNames = trueNode.ast_info.defs_local in 
      let fNames = falseNode.ast_info.defs_local in 
      let mergeNames = PSet.to_list $ PSet.union tNames fNames in  
      let mergeIds = ID.gen_fresh_list (List.length mergeNames) in
      let trueCodegen = new SSA_Codegen.ssa_codegen in 
      let falseCodegen = new SSA_Codegen.ssa_codegen in 
      (* is this If an expression or an effectful statement? *)
      let gate = (match value_id with 
      | Some valId -> 
          let trueRetId = ID.gen () in
          let trueEnv : Env.t = 
            translate_stmt condEnv codegen ~value_id:trueRetId trueNode 
          in
          let trueIds = List.map (lookup_ssa_id trueEnv) mergeNames in
          let falseRetId = ID.gen() in  
          let falseEnv = 
            translate_stmt condEnv codegen ~value_id:falseRetId falseNode 
          in
          let falseIds = List.map (lookup_ssa_id falseEnv) mergeNames in
          { 
            if_output_ids = valId :: mergeIds;
            true_ids = trueRetId :: trueIds;
            false_ids = falseRetId :: falseIds;
          }  
          
      | None ->
          (* note that here we're not passing any return/value IDs *) 
          let trueEnv = translate_stmt condEnv trueCodegen trueNode in
          let falseEnv = translate_stmt condEnv falseCodegen falseNode in
          { 
            if_output_ids = mergeIds;
            true_ids = List.map (lookup_ssa_id trueEnv) mergeNames;
            false_ids =List.map (lookup_ssa_id falseEnv) mergeNames;
          } 
      )
      in 
      let trueBlock = trueCodegen#finalize in 
      let falseBlock = falseCodegen#finalize in     
      codegen#emit [mk_stmt $ If(condVal,trueBlock,falseBlock, gate)];
      add_data_bindings env mergeNames mergeIds
   
  | AST.Def(name, rhs) -> 
      let id = ID.gen () in 
      let rhsEnv, rhsExp = translate_exp env codegen rhs in 
      (match value_id with 
        | Some valId -> 
          codegen#emit [
            mk_set [id] rhsExp; mk_set [valId] $ mk_val_exp (Var id)
          ]
        | None ->  codegen#emit [mk_stmt $ Set([id], rhsExp)]
      );
      add_data_binding rhsEnv name id  
       
  | AST.Block [] ->  env         
  | AST.Block [node] -> translate_stmt env codegen ?value_id node
  | AST.Block (node::nodes) ->
      let nodeEnv = translate_stmt env codegen node in
      let restNode = {node with AST.data = AST.Block nodes } in 
      translate_stmt nodeEnv codegen ?value_id restNode 
  | AST.SetIdx(name, indices, rhs) -> failwith "setidx not implemented"
  | AST.WhileLoop(cond,code) -> failwith "while loop not implemented"
  | AST.CountLoop(upper,code) -> failwith "count loop not implemented"
  | simple ->  
      let env', exp = translate_exp env codegen node in
      (* simple values shouldn't contain any statements *) 
      (match value_id with 
        | Some valId -> codegen#emit [mk_stmt $ Set([valId], exp)]
        | None -> codegen#emit [mk_stmt $ Set([], exp)]
      );
      env'

and translate_exp env codegen node =
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
    env, expNode 
     
  in  
  match node.data with 
  | AST.Var name -> value $ lookup_ssa env name 
  | AST.Prim p -> value (Prim p)
  | AST.Num n -> value (Num n)
  | AST.Str s -> value (Str s)
  | AST.Sym s -> value (Sym s)
  | AST.Void -> value Unit 
  | AST.App (fn, args) ->
      let fnEnv, fn' = translate_value env codegen fn in 
      let argEnv, args' = translate_args fnEnv codegen args in
      let app' = mk_exp $ App(fn', args') in
      argEnv, app'
  (* TODO: lambda lift here *) 
  | AST.Lam (vars, body) -> failwith "lambda lifting not implemented"
       
  | AST.Arr args -> 
      let ssaEnv, ssaArgs = translate_args env codegen args in 
      ssaEnv, SSA.mk_arr ssaArgs
  | AST.If _  -> failwith "unexpected If while converting to SSA"
  | AST.Def _ -> failwith "unexpected Def while converting to SSA"
  | AST.SetIdx _ -> failwith "unexpected SetIdx while converting to SSA"
  | AST.Block _  -> failwith "unexpected Block while converting to SSA"
  | AST.WhileLoop _ -> failwith "unexpected WhileLoop while converting to SSA"
  | AST.CountLoop _ -> failwith "unexpected CountLoop while converting to SSA"
   
                    
and translate_value env codegen node =
  (* simple values generate no statements and don't modify the environment *)  
  let return v = env, mk_val v in 
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
      let env' = translate_stmt env codegen ~value_id:tempId node in 
      env', mk_val $ Var tempId
        
and translate_args env codegen = function 
  | [] ->  env (* same env *) , [] (* no values *)   
  | arg::args -> 
      let currEnv, v = translate_value env codegen arg in 
      let restEnv, vs = translate_args currEnv codegen args in 
      restEnv, v :: vs 
      
      
(* given the arg names and AST body of function, generate its SSA fundef *)     
and translate_fn parentEnv argNames body = 
  let retId = ID.gen () in
  let nargs = List.length argNames in 
  let argIds = ID.gen_fresh_list nargs in
  (* map string names to their SSA identifiers -- 
     assume globalMap contains only functions 
  *)  
  let initEnv = extend parentEnv argNames argIds in
  let codegen = new ssa_codegen in   
  let _ = translate_stmt initEnv codegen ~value_id:retId body in
  (* make an empty type env since this function hasn't been typed yet *) 
  SSA.mk_fundef 
    ~body:(codegen#finalize) 
    ~tenv:ID.Map.empty 
    ~input_ids:argIds 
    ~output_ids:[retId]  