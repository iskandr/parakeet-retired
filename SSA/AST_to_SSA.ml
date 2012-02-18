(* pp: -parser o pa_macro.cmo *)

open AST
open Base
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

  let rec add_list env names ids : t = match names, ids with
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
  | GlobalScope fnLookup -> GlobalFn (fnLookup name)

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

let rec flatten_indexing (astNode:AST.node) : AST.node list =
  match astNode.data with
  | AST.App({AST.data = AST.Prim (Prim.ArrayOp Prim.Index)}, [lhs;rhs]) ->
    (flatten_indexing lhs) @ [rhs]
  | _ -> [astNode]

let get_function_id node =
  match node.data with
    | AST.Var fnName ->
      if FnManager.have_untyped_function fnName then
        FnManager.get_untyped_id fnName
      else
        failwith ("Couldn't find untyped function: " ^ fnName)
    | _ -> failwith ("Expected function name, got: " ^ (AST.to_str node))

let rec translate_exp
          (env:Env.t)
          (codegen:SSA_Codegen.codegen)
          (node : AST.node) : SSA.exp_node =

  (* simple values generate no statements and don't modify the env *)
  let value v =
    {
      exp = Values [SSA_Helpers.wrap_value v];
      exp_src=Some node.src;
      exp_types=[Type.BottomT]
    }
  in
  match node.data with
  | AST.Var name -> value $ Env.lookup env name
  | AST.Prim p -> value (Prim p)
  | AST.Num n -> value (Num n)
  | AST.Str s -> value (Str s)
  | AST.Void -> value Unit
  | AST.App (fn, args) -> translate_app env codegen fn args node.src
  (* TODO: lambda lift here *)
  | AST.Lam (vars, body) -> failwith "lambda lifting not implemented"
  | AST.Arr args ->
      let ssaArgs = translate_values env codegen args in
      SSA_Helpers.arr ssaArgs
  | AST.If _  -> failwith "unexpected If while converting to SSA"
  | AST.Assign _ -> failwith "unexpected Assign while converting to SSA"
  | AST.Block _  -> failwith "unexpected Block while converting to SSA"
  | AST.WhileLoop _ -> failwith "unexpected WhileLoop while converting to SSA"
  | AST.CountLoop _ -> failwith "unexpected CountLoop while converting to SSA"

and translate_value env codegen node : SSA.value_node =
  (* simple values generate no statements and don't modify the environment *)
  match node.AST.data with
  | AST.Var name -> SSA_Helpers.wrap_value (Env.lookup env name)
  | AST.Prim p -> SSA_Helpers.wrap_value (Prim p)
  | AST.Num n -> SSA_Helpers.wrap_value (Num n)
  | AST.Str s -> SSA_Helpers.wrap_value (Str s)
  (* anything not an immediate value must be evaluated in its own statement
     and then named
  *)
  | _ -> exp_as_value env codegen "temp" node
and translate_values env codegen nodes =
  List.map (translate_value env codegen) nodes
and translate_axes env codegen {data} = match data with
  | AST.Arr axes -> Some (translate_values env codegen axes)
  | _ -> None
and translate_adverb env codegen adverb args src =
  match adverb with
  | Prim.Map ->
    (match args with
      | fn::{data=AST.Arr arrayArgs}::{data=AST.Arr fixedArgs}::[axes] ->
        let fnId = get_function_id fn in
        let ssaFn = FnManager.get_untyped_function fnId in
        let adverbArgs = {
          SSA.init = None;
          axes = translate_axes env codegen axes;
          args = translate_values env codegen arrayArgs;
        }
        in
        let closure = {
          SSA.closure_fn = fnId;
          closure_args = translate_values env codegen fixedArgs;
          closure_arg_types = List.map (fun _ -> Type.BottomT) fixedArgs;
        }
        in
        {
          SSA.exp = SSA.Adverb(Prim.Map, closure, adverbArgs);
          exp_src = Some src;
          exp_types = List.map (fun _ -> Type.BottomT) ssaFn.SSA.fn_output_types
        }
      | _ -> failwith "Unexpected function arguments to Map"
    )
  | Prim.Reduce ->
    (match args with
     | fn::{data=AST.Arr arrayArgs}::{data=AST.Arr fixedArgs}::[axes;_] ->
        let fnId = get_function_id fn in
        let ssaFn = FnManager.get_untyped_function fnId in
        let adverbArgs = {
          SSA.init = None;
          axes = translate_axes env codegen axes;
          args = translate_values env codegen arrayArgs;
        }
        in
        let closure = {
          SSA.closure_fn = fnId;
          closure_args = translate_values env codegen fixedArgs;
          closure_arg_types = List.map (fun _ -> Type.BottomT) fixedArgs;
        }
        in
        {
          SSA.exp = SSA.Adverb(Prim.Reduce, closure, adverbArgs);
          exp_src = Some src;
          exp_types = List.map (fun _ -> Type.BottomT) ssaFn.SSA.fn_output_types
        }
    )
  | _ ->
    failwith ("Adverb not yet supported " ^ (Prim.adverb_to_str adverb))

and translate_app env codegen fn args src = match fn.data with
  | AST.Prim (Prim.Adverb adverb) ->
    translate_adverb env codegen adverb args src
  | _ ->
    let ssaFn : SSA.value_node = translate_value env codegen fn in
    let ssaArgs : SSA.value_node list = translate_values env codegen args in
    {
      exp= App(ssaFn, ssaArgs);
      exp_src=Some src;
      exp_types=[Type.BottomT]
    }
(* recursively flatten subexpressions and return last expression *)
(* as a value *)
and exp_as_value env codegen prefix node : SSA.value_node =
  let id = ID.gen_named prefix in
  let expNode = translate_exp env codegen node in
  codegen#emit [SSA_Helpers.set [id] expNode];
  SSA_Helpers.var id

let rec exps_as_values env codegen prefix nodes : SSA.value_node list =
  List.map (exp_as_value env codegen prefix) nodes

let rec collect_assignment_names = function
  | [] -> []
  | {data=AST.Var name}::rest -> name :: (collect_assignment_names rest)
  | other::_ -> failwith $ "[AST_to_SSA] Unexpected LHS " ^ (AST.to_str other)

let translate_assignment env codegen (lhs:AST.node list) rhs : Env.t =
  match lhs with
  (* if the first element of the lhs is a variable assume they all are *)
  | {AST.data=AST.Var _}::_ ->
    let names = collect_assignment_names lhs in
    let ids = List.map ID.gen_named names in
    let rhsExp = translate_exp env codegen rhs in
    codegen#emit [SSA_Helpers.stmt $ Set(ids, rhsExp)];
    Env.add_list env names ids
  | [{AST.data=AST.App({data=AST.Prim (Prim.ArrayOp Prim.Index)}, _)} as lhs] ->
    let rhs = translate_value env codegen rhs in
    let allIndices : AST.node list  = flatten_indexing lhs in
    let lhsList = translate_values env codegen allIndices in
    begin match lhsList with
      | varNode::indices ->
        codegen#emit [SSA_Helpers.setidx varNode indices rhs];
        env
      | _ -> failwith $ Printf.sprintf
        "[AST_to_SSA] Unexpected indexing arguments: %s"
        (SSA.value_nodes_to_str lhsList)
    end
  | _ -> failwith $ Printf.sprintf "Unexpected LHS of assignment: %s"
    (AST.args_to_str lhs)

let rec translate_stmt
          (env:Env.t)
          (codegen:SSA_Codegen.codegen)
          (retIds : ID.t list)
          (node : AST.node) : Env.t =
  let mk_stmt s = SSA_Helpers.stmt ~src:node.AST.src s in
  match node.AST.data with
  | AST.Return nodes ->
      if List.length nodes <> List.length retIds then
        failwith $ Printf.sprintf
          "Cannot return %d values, function expects %d"
          (List.length nodes)
          (List.length retIds)
      else
      let rhs : SSA.exp_node =
        SSA_Helpers.exp (SSA.Values (translate_values env codegen nodes))
      in
      codegen#emit [SSA_Helpers.set retIds rhs];
      env

  | AST.If(cond, trueNode, falseNode) ->
      let condVal = exp_as_value env codegen "cond" cond  in
      let tNames : string PSet.t =  AST.defs trueNode in
      let fNames : string PSet.t = AST.defs falseNode in
      let mergeNames = PSet.to_list $ PSet.union tNames fNames in
      (* for now always include retIds in case we have to return *)
      let mergeIds = retIds @ ID.gen_fresh_list (List.length mergeNames) in
      let trueCodegen = new SSA_Codegen.codegen in
      let falseCodegen = new SSA_Codegen.codegen in
      (* if we need to return a particular value, we track the returned
         value along each branch and then set the return var to be the
         SSA merge of the two branch values
      *)
      let trueEnv = translate_stmt env trueCodegen retIds trueNode in
      let trueIds = List.map (Env.lookup_id trueEnv) mergeNames in
      let falseEnv = translate_stmt env falseCodegen retIds falseNode in
      let falseIds = List.map (Env.lookup_id falseEnv) mergeNames in
      let phiNodes = SSA_Helpers.phi_nodes mergeIds trueIds falseIds in
      let trueBlock = trueCodegen#finalize in
      let falseBlock = falseCodegen#finalize in
      codegen#emit [mk_stmt $ If(condVal,trueBlock,falseBlock, phiNodes)];
      Env.add_list env mergeNames mergeIds

  | AST.Assign(lhsList, rhs) -> translate_assignment env codegen lhsList rhs

  | AST.Block nodes  -> translate_block env codegen retIds nodes
  | AST.WhileLoop(cond,body) ->
        (* FIX: I don't think this properly handles SSA gates for variables
           modified in the cond block
        *)
      let bodyCodegen = new SSA_Codegen.codegen in
      (* update the body codegen and generate a loop gate *)
      let header, exitEnv = translate_loop_body env bodyCodegen retIds body  in
      let ssaBody = bodyCodegen#finalize in
      let condCodegen = new SSA_Codegen.codegen in
      let condVal = exp_as_value exitEnv condCodegen "cond" cond in
      let condBlock = condCodegen#finalize in
      codegen#emit [
        mk_stmt $ WhileLoop(condBlock, condVal, ssaBody, header)

      ];
      env

  | AST.CountLoop(upper,body) ->
    (* store the upper loop limit in a fresh ID *)
      let upperVal = exp_as_value env codegen "upper" upper in

      (* SSA form requires the counter variable to carry three distinct IDs:
           - before the loop starts
           - merge information at the beginning of the loop
           - update counter value
      *)
      let initCounterId = ID.gen_named "init_counter" in
      let initCounterVar = SSA_Helpers.var initCounterId in
      let startCounterId = ID.gen_named "start_counter" in
      let startCounterVar = SSA_Helpers.var startCounterId in
      let endCounterId = ID.gen_named "end_counter" in
      let endCounterVar = SSA_Helpers.var endCounterId in
      (* initialize loop counter to 1 *)
      codegen#emit [SSA_Helpers.set_int initCounterId 0l];
      let condId = ID.gen_named "cond" in
      let condBlock =
        Block.singleton $
          SSA_Helpers.set [condId]
            (SSA_Helpers.app SSA_Helpers.lt [startCounterVar; upperVal])
      in
      let bodyCodegen = new SSA_Codegen.codegen in
      (* update the body codegen and generate a loop gate *)
      let header, exitEnv = translate_loop_body env bodyCodegen retIds body  in
      (* incremenet counter and add SSA gate for counter to loopGate *)
      bodyCodegen#emit [SSA_Helpers.incr endCounterId startCounterVar];

      let header' =
        (SSA_Helpers.phi startCounterId initCounterVar endCounterVar) :: header
      in
      let condVal = SSA_Helpers.var condId in
      let ssaBody = bodyCodegen#finalize in
      codegen#emit [mk_stmt $ WhileLoop(condBlock, condVal,  ssaBody, header')];
      exitEnv
  | _ ->
      failwith $ Printf.sprintf
        "[AST_to_SSA] Expected statement, received %s"
        (AST.to_str node)

and translate_loop_body envBefore codegen retIds  body : phi_nodes * Env.t =
  (* FIX: use a better AST_Info without all this local/global junk *)
  let bodyDefs : string PSet.t = AST.defs body in
  let bodyUses : string PSet.t = AST.uses body in

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
  let envOverlap = Env.add_list envBefore overlapList overlapIds in
  (* translate the body of the loop, using SSA IDs of variables
     defined before loop and the recently created IDs of variables
     which feed back into the loop
  *)
  let envEnd : Env.t = translate_stmt envOverlap codegen retIds body in
  let mk_header_phi name =
    let prevId = Env.lookup_id envBefore name in
    let startId = Env.lookup_id envOverlap name in
    let loopEndId = Env.lookup_id envEnd name in
    SSA_Helpers.phi startId (SSA_Helpers.var prevId) (SSA_Helpers.var loopEndId)

  in
  let needsPhi = List.filter (Env.mem envBefore) overlapList in
  let loopHeader = List.map mk_header_phi needsPhi in
  loopHeader , envOverlap

and translate_block env codegen retIds = function
  | [] -> env
  | [lastNode] -> translate_stmt env codegen retIds lastNode
  | node::nodes ->
    let nodeEnv = translate_stmt env codegen retIds node in
    translate_block nodeEnv codegen retIds nodes

(* given the arg names and AST body of function, generate its SSA fundef *)
and translate_fn ?name parentEnv argNames (body:AST.node) : SSA.fn =
  (* if no return statements in function, assume it returns nothing *)
  let returnArity = match body.ast_info.return_arity with
    | Some x -> x
    | None -> 0
  in
  let retIds = ID.gen_named_list "ret" returnArity in
  let argIds = List.map ID.gen_named argNames in
  (* map string names to their SSA identifiers --
     assume globalMap contains only functions
  *)
  let initEnv = Env.extend parentEnv argNames argIds in
  let codegen = new codegen in
  let _ = translate_stmt initEnv codegen retIds body in
  (* make an empty type env since this function hasn't been typed yet *)
  let body = codegen#finalize in
  SSA_Helpers.mk_fn ?name ?tenv:None ~body ~input_ids:argIds ~output_ids:retIds

