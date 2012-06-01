(* pp: -parser o pa_macro.cmo *)

open AST
open Base
open UntypedSSA

module StrSet = Set.Make(String)

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
  | AST.Call(
      {AST.data = AST.Prim (Prim.ArrayOp Prim.Index)}, 
      {Args.values = [lhs;rhs]; keywords}) -> 
    assert (keywords = []);
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
  (block:UntypedSSA.block)
  (node : AST.node) : UntypedSSA.exp_node =
  (* simple values generate no statements and don't modify the env *)
  let value v =
    {
      exp = Values [UntypedSSA.wrap_value v];
      exp_src=Some node.src;
    }
  in
  match node.data with
  | AST.Var name -> value $ Env.lookup env name
  | AST.Prim p -> value (UntypedSSA.Prim p)
  | AST.Num n -> value (UntypedSSA.Num n)
  | AST.NoneVal () -> value UntypedSSA.NoneVal
  | AST.Type _ -> failwith "Types as values not implemented"
  | AST.Tuple elts -> 
    { 
      UntypedSSA.exp = Tuple (translate_values env block elts); 
      exp_src = Some node.src;
    }  
  | AST.Call (fn, args) -> translate_app env block fn args node.src
  (* TODO: lambda lift here *)
  | AST.Lambda (args, body) -> failwith "lambda lifting not implemented"
  | AST.Array args ->
    {
      UntypedSSA.exp = UntypedSSA.Array (translate_values env block args);
      exp_src = Some node.src;
    }
  | AST.If _  -> failwith "unexpected If while converting to SSA"
  | AST.Assign _ -> failwith "unexpected Assign while converting to SSA"
  | AST.Block _  -> failwith "unexpected Block while converting to SSA"
  | AST.WhileLoop _ -> failwith "unexpected WhileLoop while converting to SSA"
  | AST.CountLoop _ -> failwith "unexpected CountLoop while converting to SSA"

and translate_value env block node : UntypedSSA.value_node =
  (* simple values generate no statements and don't modify the environment *)
  match node.AST.data with
  | AST.Var name -> UntypedSSA.wrap_value (Env.lookup env name)
  | AST.Prim p -> UntypedSSA.wrap_value (UntypedSSA.Prim p)
  | AST.Num n -> UntypedSSA.wrap_value (UntypedSSA.Num n)
  | AST.NoneVal () -> UntypedSSA.wrap_value UntypedSSA.NoneVal
  (* anything not an immediate value must be evaluated in its own statement
     and then named
  *)
  | _ -> 
    exp_as_value env block "temp" node
and translate_values env block nodes =
  List.map (translate_value env block) nodes

and translate_axes env block astNode = match astNode.data with
  | AST.Tuple axes 
  | AST.Array axes -> Some (translate_values env block axes)
  | AST.NoneVal () -> None
  | _ -> Some ([translate_value env block astNode])
  (*
  | AST.Num n -> Some ([UntypedSSA.ValueHelpers.num ~src:astNode.src n])
  *) 
  (*| other ->
    failwith $ Printf.sprintf "Unrecognized axis arg (must be constant): %s" 
    (AST.to_str astNode)
  *)
and translate_adverb env block adverb 
  (args :  AST.node Args.actual_args) (src:SrcInfo.t) =
  match args with
  | {Args.values=fn::arrayArgs; keywords} -> 
    let fixedArgs = 
      match List.assoc_option "fixed" keywords with 
      | Some {data = AST.Tuple fixedArgs} -> fixedArgs
      | None -> []
    in 
    let optAxes = 
      match List.assoc_option "axis" keywords with 
      | Some axes -> translate_axes env block axes
      | None -> 
        (match List.assoc_option "axes" keywords with 
         | Some axes -> translate_axes env block axes 
         | None -> None
        ) 
    in 
    let adverbInfo = {
      Adverb.adverb = adverb;
      adverb_fn = translate_value env block fn;
      axes = optAxes;
      array_args = translate_values env block arrayArgs;
      fixed_args = translate_values env block fixedArgs;
      init = None;
    }
    in
    {
      UntypedSSA.exp = UntypedSSA.Adverb adverbInfo;
      exp_src = Some src
    }
  | _ -> failwith "Malformed adverb args"

and translate_app env block fn 
  (args: AST.node Args.actual_args) (src:SrcInfo.t) = 
  match fn.data with
  | AST.Prim (Prim.Adverb adverb) -> translate_adverb env block adverb args src
  | _ ->
    let ssaFn : UntypedSSA.value_node = translate_value env block fn in
    let ssaArgs : UntypedSSA.value_node Args.actual_args = 
      Args.apply_to_actual_values (translate_value env block) args 
    in
    {
      exp= Call(ssaFn, ssaArgs);
      exp_src=Some src;
    }
(* recursively flatten subexpressions and return last expression *)
(* as a value *)
and exp_as_value env block prefix node : UntypedSSA.value_node =
  let id = ID.gen_named prefix in
  let expNode = translate_exp env block node in
  Block.add block (UntypedSSA.set [id] expNode);
  UntypedSSA.var id

let rec exps_as_values env block prefix nodes : UntypedSSA.value_node list =
  List.map (exp_as_value env block prefix) nodes

let rec collect_assignment_names = function
  | [] -> []
  | {data=AST.Var name}::rest -> name :: (collect_assignment_names rest)
  | other::_ -> failwith $ "[AST_to_SSA] Unexpected LHS " ^ (AST.to_str other)

let translate_assignment env block (lhs:AST.node list) rhs : Env.t =
  match lhs with
  (* if the first element of the lhs is a variable assume they all are *)
  | {AST.data=AST.Var _}::_ ->
    let names = collect_assignment_names lhs in
    let ids = List.map ID.gen_named names in
    let rhsExp = translate_exp env block rhs in
    Block.add block (UntypedSSA.stmt $ Set(ids, rhsExp));
    Env.add_list env names ids
  | [{AST.data=AST.Call(
        {data=AST.Prim (Prim.ArrayOp Prim.Index)}, _)} as lhs] ->
    let rhs = translate_exp env block rhs in
    let allIndices : AST.node list  = flatten_indexing lhs in
    let lhsList = translate_values env block allIndices in
    begin match lhsList with
      | varNode::indices ->
        Block.add block (UntypedSSA.setidx varNode indices rhs);
        env
      | _ -> failwith $ Printf.sprintf
        "[AST_to_SSA] Unexpected indexing arguments: %s"
        (UntypedSSA.value_nodes_to_str lhsList)
    end
  | _ -> 
    failwith $ 
      Printf.sprintf "Unexpected LHS of assignment: %s"
      (AST.nodes_to_str lhs)

let rec translate_stmt
          (env:Env.t)
          (block:UntypedSSA.block)
          (retIds : ID.t list)
          (node : AST.node) : Env.t =
  let src = node.AST.src in
  let mk_stmt s = UntypedSSA.stmt ~src s in
  match node.AST.data with
  | AST.Return nodes ->
    if List.length nodes <> List.length retIds then
      failwith $ Printf.sprintf
        "Cannot return %d values, function expects %d"
        (List.length nodes)
        (List.length retIds)
    else
    let rhs : UntypedSSA.exp_node =
      {
        UntypedSSA.exp =
          UntypedSSA.Values (translate_values env block nodes);
        UntypedSSA.exp_src = Some src;
      }
    in
    Block.add block (UntypedSSA.set retIds rhs);
    env

  | AST.If(cond, trueNode, falseNode) ->
    let condVal = exp_as_value env block "cond" cond  in
    let tNames =  AST.defs trueNode in
    let fNames = AST.defs falseNode in
    let mergeNames = StrSet.to_list $ StrSet.union tNames fNames in
    (* for now always include retIds in case we have to return *)
    let mergeIds = List.map ID.gen_named mergeNames in
    let trueBlock : UntypedSSA.block = Block.create() in
    let falseBlock : UntypedSSA.block = Block.create () in
    (* if we need to return a particular value, we track the returned
       value along each branch and then set the return var to be the
       SSA merge of the two branch values
    *)
    let trueEnv = translate_stmt env trueBlock retIds trueNode in
    let trueIds = List.map (Env.lookup_id trueEnv) mergeNames in
    let falseEnv = translate_stmt env falseBlock retIds falseNode in
    let falseIds = List.map (Env.lookup_id falseEnv) mergeNames in
    let phiLefts = List.map UntypedSSA.var trueIds in
    let phiRights = List.map UntypedSSA.var falseIds in
    let phiNodes = PhiNode.mk_list mergeIds phiLefts phiRights in
    Block.add block (mk_stmt $ If(condVal, trueBlock, falseBlock, phiNodes));
    Env.add_list env mergeNames mergeIds

  | AST.Assign(lhsList, rhs) -> translate_assignment env block lhsList rhs

  | AST.Block nodes  -> translate_block env block retIds nodes
  | AST.WhileLoop(cond,body) ->
    (* FIX: I don't think this properly handles SSA gates for variables
       modified in the cond block
    *)

    let bodyBlock : UntypedSSA.block = Block.create () in
    (* update the body block and generate a loop gate *)
    let header, exitEnv = translate_loop_body env bodyBlock retIds body  in
       let condBlock : UntypedSSA.block = Block.create () in
    let condVal = exp_as_value exitEnv condBlock "cond" cond in
    Block.add block
      (mk_stmt $ WhileLoop(condBlock, condVal, bodyBlock, header));
    exitEnv

  | AST.CountLoop(upper,body) ->
    (* store the upper loop limit in a fresh ID *)
      let upperVal = exp_as_value env block "upper" upper in

      (* SSA form requires the counter variable to carry three distinct IDs:
           - before the loop starts
           - merge information at the beginning of the loop
           - update counter value
      *)
      let initCounterId = ID.gen_named "init_counter" in
      let initCounterVar = UntypedSSA.var initCounterId in
      let startCounterId = ID.gen_named "start_counter" in
      let startCounterVar = UntypedSSA.var startCounterId in
      let init = wrap_exp $ UntypedSSA.int32 0 in 
    (* (UntypedSSA.Num (ParNum.Int32 0L)) in
      *)
      Block.add block (UntypedSSA.set [initCounterId] init);
      let condId = ID.gen_named "cond" in
      let callExp = 
        UntypedSSA.Call(
          UntypedSSA.lt, 
          Args.of_values [startCounterVar; upperVal]) 
      in
      let test =
        { UntypedSSA.exp = callExp; 
          exp_src = Some src;
        }
      in
      let condBlock = Block.singleton (UntypedSSA.set [condId] test) in
      let bodyBlock = Block.create () in
      (* update the body block and generate a loop gate *)
      let header, exitEnv = translate_loop_body env bodyBlock retIds body  in
      (* incremenet counter and add SSA gate for counter to loopGate *)
      let endCounterId = ID.gen_named "end_counter" in
      let endCounterVar = UntypedSSA.var endCounterId in
      let incrExp = 
        UntypedSSA.Call(
          UntypedSSA.plus, 
          Args.of_values [startCounterVar; UntypedSSA.one])
      in 
      let incrExpNode : UntypedSSA.exp_node =
        { UntypedSSA.exp = incrExp;
          exp_src = Some src;
        }
      in
      Block.add bodyBlock (UntypedSSA.set [endCounterId] incrExpNode);
      let header' =
        (PhiNode.mk startCounterId initCounterVar endCounterVar) :: header
      in
      let condVal = UntypedSSA.var condId in
      Block.add block
        (mk_stmt $ WhileLoop(condBlock, condVal, bodyBlock, header'))
      ;
      exitEnv
  | _ ->
      failwith $ Printf.sprintf
        "[AST_to_SSA] Expected statement, received %s"
        (AST.to_str node)

and translate_loop_body envBefore block retIds  body : phi_nodes * Env.t =
  (* FIX: use a better AST_Info without all this local/global junk *)
  let bodyDefs = AST.defs body in
  let bodyUses = AST.uses body in

  (* At the end of the loop,
     what are the IDs of variables which are both read and written to?
  *)
  let overlap  = StrSet.inter bodyDefs bodyUses in
  let overlapList : string list = StrSet.to_list overlap in
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
  let envEnd : Env.t = translate_stmt envOverlap block retIds body in
  let mk_header_phi name =
    let prevId = Env.lookup_id envBefore name in
    let startId = Env.lookup_id envOverlap name in
    let loopEndId = Env.lookup_id envEnd name in
    PhiNode.mk startId (UntypedSSA.var prevId) (UntypedSSA.var loopEndId)

  in
  let needsPhi = List.filter (Env.mem envBefore) overlapList in
  let loopHeader = List.map mk_header_phi needsPhi in
  loopHeader , envOverlap

and translate_block env block retIds = function
  | [] -> env
  | [lastNode] -> translate_stmt env block retIds lastNode
  | node::nodes ->
    let nodeEnv = translate_stmt env block retIds node in
    translate_block nodeEnv block retIds nodes

(* given the arg names and AST body of function, generate its SSA fundef *)
and translate_fn 
    ?name 
    parentEnv 
    (args:AST.node Args.formal_args) 
    (body:AST.node) : UntypedSSA.fn =
  (* if no return statements in function, assume it returns nothing *)
  Printf.printf "[AST->SSA] translate_fn\n%!";
  let returnArity = match body.ast_info.return_arity with
    | Some x -> x
    | None -> 0
  in
  Printf.printf "[AST->SSA] Generating return IDs\n%!";
  let retIds = ID.gen_named_list "ret" returnArity in
  let argNames = Args.all_formal_names args in 
  Printf.printf "[AST_to_SSA] Formal args: %s\n%!" 
    (String.concat ", " argNames); 
  let argIds =  List.map ID.gen_named argNames in
  Printf.printf "[AST_to_SSA] Creating name->id mapping\n%!";  
  let argNamesToIds = String.Map.of_lists argNames argIds in 
  (* map string names to their SSA identifiers --
     assume globalMap contains only functions
  *)
  Printf.printf "[AST_to_SSA] Extending env\n%!";
  let initEnv = Env.extend parentEnv argNames argIds in
  Printf.printf "[AST_to_SSA] Creating body block\n%!";
  let typedBlock : UntypedSSA.block = Block.create () in
  Printf.printf "[AST_to_SSA] Initializing formal args\n%!";
  let ssaArgs : UntypedSSA.value_node Args.formal_args  = 
    Args.apply_to_formal_values 
      (translate_value initEnv typedBlock)
      args
  in 
  Printf.printf "[AST_to_SSA] Starting translation of body\n%!";
  let _ = translate_stmt initEnv typedBlock retIds body in
  Printf.printf "[AST_to_SSA] Making fn\n%!";
  UntypedSSA.mk_fn
    ?name
    ~body:typedBlock
    ~inputs:ssaArgs
    ~input_names_to_ids:argNamesToIds
    ~output_ids:retIds

