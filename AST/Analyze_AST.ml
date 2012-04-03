open Base
open AST
open Prim
open Printf

(* FIX: for now, we're reusing the Analyze_AST module used by
   the Q preprocessor,
   Problems:
     - it uses linear-time PSet instead of log-time String.Set
     - it's slow and poorly implemented
     - it wastes a lot of time computing sets of volatile  variables
     which aren't relevant outside the Q preprocessor
   It does, however, populate the AST info fields with info about
   uses and defs later used by AST_to_SSA
*)

module StrSet = Set.Make(String)

type scope_info = {
  volatile_local : StrSet.t;
  volatile_global : StrSet.t;
  locals : StrSet.t;
}

let combine_scope_info s1 s2 = {
  volatile_local = StrSet.union s1.volatile_local s2.volatile_local;
  volatile_global = StrSet.union s1.volatile_global s2.volatile_global;
  locals = StrSet.union s1.locals s2.locals;
}

let emptyScopeInfo = {
  volatile_local = StrSet.empty;
  volatile_global = StrSet.empty;
  locals = StrSet.empty;
}

let rec get_assignment_name node = match node.data with
  | Var name -> Some name
  | Prim (Prim.ArrayOp Prim.Index) -> None
  | App (lhs, _ ) -> get_assignment_name lhs
  | _ -> failwith $ Printf.sprintf
        "Unexpected AST node on LHS of assignment: %s"
        (AST.to_str node)

let rec get_assignment_names = function
  | [] -> []
  | node::nodes ->
    let rest = get_assignment_names nodes in
    begin match get_assignment_name node with
      | Some name -> name :: rest
      | None -> rest
    end

let rec fold_block ~inFunction scopeInfo blockInfo = function
  | [] -> scopeInfo, blockInfo
  | node::nodes ->
    let scopeInfo' = analyze_node ~inFunction scopeInfo node in
    let blockInfo' = combine_ast_info node.ast_info blockInfo in
    fold_block ~inFunction scopeInfo' blockInfo' nodes

and analyze_block ~inFunction scopeInfo nodes =
  let emptyBlockInfo = mk_ast_info () in
  fold_block ~inFunction scopeInfo emptyBlockInfo nodes

and analyze_node ~inFunction scopeInfo node =
  match node.data with
  | Lam (ids, body) ->
    node.ast_info.is_function <- true;
    node.ast_info.defs_local <- StrSet.of_list ids;
    if inFunction then node.ast_info.nested_functions <- true;
    let initScopeInfo = {emptyScopeInfo with locals = StrSet.of_list ids} in
    let bodyScopeInfo = analyze_node initScopeInfo ~inFunction:true body in
    node.ast_info <- combine_ast_info node.ast_info body.ast_info;
    let bodyVolatile = bodyScopeInfo.volatile_global in
    { scopeInfo with
      volatile_global = StrSet.union scopeInfo.volatile_global bodyVolatile
    }
  | CountLoop (a,b)
  | WhileLoop (a,b) ->
    let scopeInfo', astInfo' =  analyze_block ~inFunction scopeInfo [a;b] in
    node.ast_info <- astInfo';
    scopeInfo'
  | Arr nodes
  | Block nodes ->
    let scopeInfo', astInfo' = analyze_block ~inFunction scopeInfo nodes in
    node.ast_info <- astInfo';
    scopeInfo'
  | If(test, tNode, fNode) ->
    let testInfo = analyze_node ~inFunction scopeInfo test in
    let tScopeInfo = analyze_node ~inFunction testInfo tNode in
    let fScopeInfo = analyze_node ~inFunction testInfo fNode in
    node.ast_info <-
        combine_ast_info test.ast_info
        (combine_ast_info tNode.ast_info fNode.ast_info);
    {
      volatile_local =
        StrSet.union tScopeInfo.volatile_local fScopeInfo.volatile_local;
      volatile_global =
        StrSet.union tScopeInfo.volatile_global fScopeInfo.volatile_global;
      locals = StrSet.inter tScopeInfo.locals fScopeInfo.locals;
    }
  | App (fn,args) ->
    let emptyArgsInfo = mk_ast_info () in
    let scopeInfo', argsInfo =
      fold_block ~inFunction scopeInfo emptyArgsInfo (List.rev args)
    in
    let scopeInfo'' = analyze_node ~inFunction scopeInfo' fn in
    node.ast_info <- combine_ast_info fn.ast_info argsInfo;
    (*
      IMPORTANT!!!
      for now assume function calls don't produce functions
    *)
    node.ast_info.is_function <- false;
    scopeInfo''
  | Var name ->
    let isLocal = StrSet.mem name scopeInfo.locals in
    if inFunction &&  isLocal then
      node.ast_info.reads_local <- StrSet.add name node.ast_info.reads_local
    else
      node.ast_info.reads_global <- StrSet.add name node.ast_info.reads_global
    ;
    scopeInfo
  | Assign (lhsList, rhs) ->
    let scopeInfo = analyze_node ~inFunction scopeInfo rhs in
    let scopeInfo, lhsAstInfo =
      analyze_block ~inFunction scopeInfo lhsList
    in
    node.ast_info <- combine_ast_info lhsAstInfo rhs.ast_info;
    begin match get_assignment_names lhsList with
    | [] -> scopeInfo
    | names ->
      let nameSet = StrSet.of_list names in
      let astInfo = node.ast_info in
      if inFunction then
        astInfo.defs_local <- StrSet.union nameSet astInfo.defs_local
      else
        astInfo.defs_global <- StrSet.union nameSet astInfo.defs_global
      ;
      let add_to_locals scopeInfo name =
        if StrSet.mem name scopeInfo.locals then (
          astInfo.writes_local <- StrSet.add name astInfo.writes_local
        ;
        { scopeInfo with
          volatile_local = StrSet.add name scopeInfo.volatile_local
        }
        )
        else { scopeInfo with locals = StrSet.add name scopeInfo.locals }
        in
        List.fold_left add_to_locals scopeInfo names
    end
  | Return args ->
    let scopeInfo, astInfo =  analyze_block ~inFunction scopeInfo args in
    node.ast_info <- astInfo;
    astInfo.return_arity <-
      combine_return_arity astInfo.return_arity (Some (List.length args))
    ;
    scopeInfo
  | Prim op ->
    if not $ is_pure_op op then node.ast_info.io <- true; scopeInfo
  | Str _
  | Num _
  | Void -> scopeInfo

(* annotates ast_info fields of all ast nodes, and returns a *)
(* set of volatile var names from the outermost scope *)
let analyze_ast ast =
  let finalScopeInfo = analyze_node ~inFunction:false emptyScopeInfo ast in
  StrSet.union finalScopeInfo.volatile_local finalScopeInfo.volatile_global
