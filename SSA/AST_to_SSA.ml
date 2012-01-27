(* pp: -parser o pa_macro.cmo *)

open Base
open AST
open SSA
open SSA_Codegen
open SSA_Helpers

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


  let rec mem env name = match env with
  | GlobalScope fnLookup ->
    (try let _ = fnLookup name in true with _ -> false)
  | LocalScope (dataMap, parent) ->
    String.Map.mem name dataMap || mem parent name

  let add env name id = match env with
  | GlobalScope _ -> failwith "can't add data binding at global scope"
  | LocalScope (dataMap, parent) ->
    LocalScope (String.Map.add name id dataMap, parent)

  let rec add_list env names ids = match names, ids with
    | [], _
    | _, [] -> env
    | name::moreNames, id::moreIds ->
        add_list (add env name id) moreNames moreIds

  let extend (parentEnv : t) (names : string list) (ids : ID.t list) : t =
    let localMap = String.Map.extend String.Map.empty names ids in
    LocalScope(localMap, parentEnv)

  (*
     Recursively lookup variable name in nested scopes, return an
     SSA value-- either Var _ for data or Fn _ for a function
  *)
  let rec lookup env name = match env with
  | LocalScope (dataEnv, parent) ->
    if String.Map.mem name dataEnv then Var (String.Map.find name dataEnv)
    else lookup parent name
  | GlobalScope fnLookup ->  GlobalFn (fnLookup name)

  (* assume that we know this name is not of a global function *)
  let lookup_id env name  =
    match lookup env name with
    | Var id -> id
    | _ -> assert false

  let rec lookup_opt env name = match env with
    | LocalScope (map, parent) ->
      if String.Map.mem name map then Some (Var (String.Map.find name map))
      else lookup_opt parent name
    | GlobalScope fnLookup ->
      (*if the lookup function fails, then name isn't a member of any scope*)
      (try Some (GlobalFn (fnLookup name)) with _ -> None)

  let lookup_id_opt env name = match lookup_opt env name with
    | Some (Var id) -> Some id
    | _ -> None

  let has_id env name = (lookup_opt env name) <> None

end

open Env


(* value_id is an optional parameter, if it's provided then the generated
   statements must set retId to the last value
*)
let rec translate_stmt
          (env:Env.t)
          (codegen:SSA_Codegen.ssa_codegen)
          ?(value_id : ID.t option)
          (node : AST.node) : Env.t =
  let mk_stmt s = mk_stmt ~src:node.AST.src s in
  match node.AST.data with
  | AST.If(cond, trueNode, falseNode) ->
      let condEnv, condVal = translate_value env codegen cond in
      let tNames =  defs trueNode in
      let fNames = defs falseNode in
      let mergeNames = PSet.to_list $ PSet.union tNames fNames in
      let mergeIds = ID.gen_fresh_list (List.length mergeNames) in
      let trueCodegen = new SSA_Codegen.ssa_codegen in
      let falseCodegen = new SSA_Codegen.ssa_codegen in
      (* if we need to return a particular value, we track the returned
         value along each branch and then set the return var to be the
         SSA merge of the two branch values
      *)
      let phiNodes = (match value_id with
      | Some valId ->
          let trueRetId = ID.gen () in
          let trueEnv : Env.t =
            translate_stmt condEnv codegen ~value_id:trueRetId trueNode
          in
          let trueIds =
            trueRetId :: List.map (lookup_id trueEnv) mergeNames
          in
          let falseRetId = ID.gen() in
          let falseEnv =
            translate_stmt condEnv codegen ~value_id:falseRetId falseNode
          in
          let falseIds =
            falseRetId :: List.map (lookup_id falseEnv) mergeNames
          in
          mk_phi_nodes (valId::mergeIds) trueIds falseIds

      | None ->
          (* note that here we're not passing any return/value IDs *)
          let trueEnv = translate_stmt condEnv trueCodegen trueNode in
          let trueIds = List.map (lookup_id trueEnv) mergeNames in
          let falseEnv = translate_stmt condEnv falseCodegen falseNode in
          let falseIds = List.map (lookup_id falseEnv) mergeNames in
          mk_phi_nodes mergeIds trueIds falseIds
      )
      in
      let trueBlock = trueCodegen#finalize in
      let falseBlock = falseCodegen#finalize in
      codegen#emit [mk_stmt $ If(condVal,trueBlock,falseBlock, phiNodes)];
      add_list env mergeNames mergeIds

  | AST.Assign({AST.data=AST.Var name}, rhs) ->
      let id = ID.gen_named name in
      let rhsEnv, rhsExp = translate_exp env codegen rhs in
      (match value_id with
        | Some valId ->
          codegen#emit [
            mk_set [id] rhsExp; mk_set [valId] $ mk_val_exp (Var id)
          ]
        | None ->  codegen#emit [mk_stmt $ Set([id], rhsExp)]
      );
      add rhsEnv name id

  | AST.Block nodes  -> translate_block env codegen ?value_id nodes
  | AST.WhileLoop(cond,body) ->
        (* FIX: I don't think this properly handles SSA gates for variables
           modified in the cond block
        *)
      let bodyCodegen = new SSA_Codegen.ssa_codegen in
      (* update the body codegen and generate a loop gate *)
      let header, exitEnv = translate_loop_body env bodyCodegen body  in
      let ssaBody = bodyCodegen#finalize in
      let condId = ID.gen_named "cond" in
      let condCodegen = new SSA_Codegen.ssa_codegen in
      let condEnv = translate_stmt exitEnv condCodegen ~value_id:condId cond in
      let condVal = mk_var condId in
      let condBlock = condCodegen#finalize in
      codegen#emit [
        mk_stmt $ WhileLoop(condBlock, condVal, ssaBody, header)

      ];
      condEnv

  | AST.CountLoop(upper,body) ->
    (* store the upper loop limit in a fresh ID *)
      let upperId = ID.gen_named "upper" in
      let env' = translate_stmt env codegen ~value_id:upperId upper in
      (* SSA form requires the counter variable to carry three distinct IDs:
           - before the loop starts
           - merge information at the beginning of the loop
           - update counter value
      *)
      let initCounterId = ID.gen_named "init_counter" in
      let initCounterVar = mk_var initCounterId in
      let startCounterId = ID.gen_named "start_counter" in
      let startCounterVar = mk_var startCounterId in
      let endCounterId = ID.gen_named "end_counter" in
      let endCounterVar = mk_var endCounterId in
      (* initialize loop counter to 1 *)
      codegen#emit [SSA_Codegen.set_int initCounterId 0l];
      let condId = ID.gen_named "cond" in
      let condBlock =
        Block.singleton $
          mk_set [condId]
            (mk_app SSA_Codegen.lt [startCounterVar; mk_var upperId])
      in
      let bodyCodegen = new SSA_Codegen.ssa_codegen in
      (* update the body codegen and generate a loop gate *)
      let header, exitEnv = translate_loop_body env' bodyCodegen body  in
      (* incremenet counter and add SSA gate for counter to loopGate *)
      bodyCodegen#emit [SSA_Codegen.incr endCounterId startCounterVar];

      let header' =
        (mk_phi startCounterId initCounterVar endCounterVar) :: header
      in
      let condVal = mk_var condId in
      let ssaBody = bodyCodegen#finalize in
      codegen#emit [
        mk_stmt $ WhileLoop(condBlock, condVal,  ssaBody, header')

      ];
      exitEnv
  | simple ->
      let env', exp = translate_exp env codegen node in
      (* simple values shouldn't contain any statements *)
      (match value_id with
        | Some valId -> codegen#emit [mk_stmt $ Set([valId], exp)]
        | None -> codegen#emit [mk_stmt $ Set([], exp)]
      );
      env'
and translate_loop_body envBefore codegen body
      : phi_nodes * Env.t =
  (* FIX: use a better AST_Info without all this local/global junk *)
  let bodyDefs = defs body in
  let bodyUses = uses body in

  (* At the end of the loop,
     what are the IDs of variables which are both read and written to?
  *)
  let overlap : string PSet.t  = PSet.inter bodyDefs bodyUses in
  let overlapList : string list = PSet.to_list overlap in
  IFDEF DEBUG THEN
        Printf.printf "[AST->SSA] Loop vars: %s\n"
            (String.concat ", " overlapList)
  ENDIF;
  let overlapIds = ID.gen_fresh_list (List.length overlapList) in
  (* give a unique ID to every variable which is both
     read and modified in the loop body
   *)
  let envOverlap = add_list envBefore overlapList overlapIds in
  (* translate the body of the loop, using SSA IDs of variables
     defined before loop and the recently created IDs of variables
     which feed back into the loop
  *)
  let envEnd : Env.t = translate_stmt envOverlap codegen body in
  let mk_header_phi name =
    let prevId = lookup_id envBefore name in
    let loopStartId = lookup_id envOverlap name in
    let loopEndId = lookup_id envEnd name in
    mk_phi loopStartId (mk_var prevId) (mk_var loopEndId)
  in
  let needsPhi = List.filter (mem envBefore) overlapList in
  let loopHeader = List.map mk_header_phi needsPhi in
  loopHeader , envOverlap

and translate_block env codegen ?value_id = function
  | [] -> env
  | [lastNode] -> translate_stmt env codegen ?value_id lastNode
  | node::nodes ->
      let nodeEnv = translate_stmt env codegen node in
      translate_block nodeEnv codegen ?value_id nodes

and translate_exp env codegen node =
  let mk_exp e =
    { exp= e; exp_src=Some node.src; exp_types=[Type.BottomT]}
  in
  (* simple values generate no statements and don't modify the env *)
  let value v =
    let expNode =
      {
        exp = Values [mk_val v];
        exp_src=Some node.src;
        exp_types=[Type.BottomT]
      }
    in
    env, expNode

  in
  match node.data with
  | AST.Var name -> value $ lookup env name
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
      ssaEnv, mk_arr ssaArgs
  | AST.If _  -> failwith "unexpected If while converting to SSA"
  | AST.Assign _ -> failwith "unexpected Assign while converting to SSA"
  | AST.Block _  -> failwith "unexpected Block while converting to SSA"
  | AST.WhileLoop _ -> failwith "unexpected WhileLoop while converting to SSA"
  | AST.CountLoop _ -> failwith "unexpected CountLoop while converting to SSA"


and translate_value env codegen node =
  (* simple values generate no statements and don't modify the environment *)
  let return v = env, mk_val v in
  match node.AST.data with
  | AST.Var name -> return $ lookup env name
  | AST.Prim p -> return $ Prim p
  | AST.Num n -> return $ Num n
  | AST.Str s -> return $ Str s
  | AST.Sym s -> return $ Sym s
  (* anything not an immediate value must be evaluated in its own statement
     and then named
  *)
  | _ ->
    let tempId = ID.gen_named "temp" in
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
  let retId = ID.gen_named "ret" in
  let argIds = List.map ID.gen_named argNames in
  (* map string names to their SSA identifiers --
     assume globalMap contains only functions
  *)
  let initEnv = extend parentEnv argNames argIds in
  let codegen = new ssa_codegen in
  let _ = translate_stmt initEnv codegen ~value_id:retId body in
  (* make an empty type env since this function hasn't been typed yet *)
  let body = codegen#finalize in
  SSA_Helpers.mk_fn ~body  ~input_ids:argIds ~output_ids:[retId]