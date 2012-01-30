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

type scope_info = {
    volatile_local : string PSet.t;
    volatile_global : string PSet.t;
    locals : string PSet.t;
}

let combine_scope_info s1 s2 = {
    volatile_local = PSet.union s1.volatile_local s2.volatile_local;
	volatile_global = PSet.union s1.volatile_global s2.volatile_global;
	locals = PSet.union s1.locals s2.locals;
}

let emptyScopeInfo = {
    volatile_local = PSet.empty;
    volatile_global = PSet.empty;
    locals = PSet.empty;
}


let rec get_assignment_name node = match node.data with
  | Var name -> Some name
  | Prim (Prim.ArrayOp Prim.Index) -> None
  | App (lhs, _ ) -> get_assignment_name lhs
  | _ -> failwith $ Printf.sprintf
      "Unexpected AST node on LHS of assignment: %s"
      (AST.to_str node)

let rec fold_block ~inFunction scopeInfo blockInfo = function
    | [] -> scopeInfo, blockInfo
	| node::nodes ->
		let scopeInfo' = analyze_node ~inFunction scopeInfo node in
		let blockInfo' = combine_ast_info node.ast_info blockInfo in
		fold_block ~inFunction scopeInfo' blockInfo' nodes

and analyze_block ~inFunction scopeInfo nodes =
    let emptyBlockInfo = mk_ast_info () in
    fold_block ~inFunction scopeInfo emptyBlockInfo nodes

and analyze_node ~inFunction scopeInfo node = match node.data with
    | Lam (ids, body) ->
        node.ast_info.is_function <- true;
        node.ast_info.defs_local <- PSet.from_list ids;
        if inFunction then node.ast_info.nested_functions <- true;
        let initScopeInfo = {emptyScopeInfo with locals = PSet.from_list ids} in
        let bodyScopeInfo = analyze_node initScopeInfo ~inFunction:true body in
        node.ast_info <- combine_ast_info node.ast_info body.ast_info;
        let bodyVolatile = bodyScopeInfo.volatile_global in
        { scopeInfo with
            volatile_global = PSet.union scopeInfo.volatile_global bodyVolatile
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
            (combine_ast_info tNode.ast_info fNode.ast_info)
        ;
        {
            volatile_local =
                PSet.union tScopeInfo.volatile_local fScopeInfo.volatile_local;
            volatile_global =
                PSet.union tScopeInfo.volatile_global fScopeInfo.volatile_global;
            locals = PSet.inter tScopeInfo.locals fScopeInfo.locals;
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
        let isLocal = PSet.mem name scopeInfo.locals in
        if inFunction &&  isLocal then
          node.ast_info.reads_local <- PSet.add name node.ast_info.reads_local
        else
          node.ast_info.reads_global <- PSet.add name node.ast_info.reads_global
        ;
        scopeInfo

    | Assign (lhs, rhs) ->
        let scopeInfo = analyze_node ~inFunction scopeInfo rhs in
        let scopeInfo = analyze_node ~inFunction scopeInfo lhs in
        node.ast_info <- combine_ast_info lhs.ast_info rhs.ast_info;
        begin match get_assignment_name lhs with
          | Some name ->
            if inFunction then (
              let locals = PSet.add name node.ast_info.defs_local in
              node.ast_info.defs_local <- locals
            )
            else (
              let globals =  PSet.add name node.ast_info.defs_global in
              node.ast_info.defs_global <- globals
            );
            if PSet.mem name scopeInfo.locals then (
              node.ast_info.writes_local <-
                PSet.add name node.ast_info.writes_local
              ;
              { scopeInfo with
                  volatile_local = PSet.add name scopeInfo.volatile_local
              }
            )
            else { scopeInfo with locals = PSet.add name scopeInfo.locals }
          | None -> scopeInfo
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
    | Sym _
    | Str _
    | Num _
    | Void -> scopeInfo



(* annotates ast_info fields of all ast nodes, and returns a *)
(* set of volatile var names from the outermost scope *)
let analyze_ast ast =
  let finalScopeInfo = analyze_node ~inFunction:false emptyScopeInfo ast in
  PSet.union finalScopeInfo.volatile_local finalScopeInfo.volatile_global


let collect_defs ast =
  let rec fold_block lastNode defs  = function
    | [] -> lastNode, defs
    | node::nodes ->
        let lastNode', defs' = aux defs node in
        fold_block lastNode' defs' nodes
  and aux defs ast = match ast.data with
   | Assign (lhs, rhs) ->
      (* collect any weird assignments that somehow *)
      (* got onto the LHS *)
      let _, defs = aux defs lhs in
      let lastNode, defs = aux defs rhs in
      (* get the name of the variable on the lhs *)
      (match get_assignment_name lhs with
        | Some name -> lastNode, (name, lastNode)::defs
        | None -> lastNode, defs
       )
   | Block nodes -> fold_block ast defs nodes
   | _ -> ast, defs
  in snd (aux [] ast)



(* takes a map of function name / ast info pairs,
   iteratively combines ast info objects by performing a
   transitive closure of the reads_global graph
*)

let transitive_closure infoMap =
   let grow key info (map, keepGoing) =
      let folder var acc =
        if PMap.mem var map then (
          (* only combine if the referenced variable is a function *)
          let varInfo = PMap.find var map  in
          if varInfo.is_function then combine_ast_info varInfo acc
          else acc
        )
        else
        (* if the variable isn't recognized as a user-defined global,
           check if it's defined in the standard library
        *)
        if FnManager.have_untyped_function var then acc
        else failwith $
          Printf.sprintf "couldn't find info for global var %s" var
      in
      let newInfo = PSet.fold folder info.reads_global info in
      let changed =  PSet.cardinal newInfo.reads_global >
                     PSet.cardinal info.reads_global in
      let keepGoing' = keepGoing || changed in
      let map' = PMap.add key newInfo map in
      (map', keepGoing')
    in
    let rec iterate accMap =
       let (accMap', keepGoing) = PMap.foldi grow accMap (accMap, false) in
       if keepGoing then iterate accMap'
       else accMap'
    in iterate infoMap

(* given a map from function names to their AST info objects,
   and a set of known volatile functions,
   iteratively expand set of unsafe functions until fixpoit.
   Return a safe/unsafe tuple of sets
*)
let find_safe_functions globalFnMap volatileFnSet =
    (* folds the set of functions into safe/unsafe sets
       by checking whether each function accesses any variable previously
       believed to be unsafe
    *)
    let rec aux prevUnsafe name (currSafe, currUnsafe) =
      if PMap.mem name globalFnMap then
        let info = PMap.find name globalFnMap in (
        if PSet.is_empty (PSet.inter info.reads_global prevUnsafe) then
          PSet.add name currSafe, currUnsafe
        else
          currSafe, PSet.add name currUnsafe
        )
      else
      (* function is in the standard library  *)
      if FnManager.have_untyped_function name then
        currSafe, currUnsafe
      else failwith $ Printf.sprintf "binding for variable %s not found " name
    in
    (* repeatedly expand unsafe set until we reach a fixpoint *)
    let rec iterate safe unsafe =
      let initAcc = PSet.empty, unsafe in
      (* partially apply 'unsafe' so that current iteration checks
         whether functions access variables in this set
      *)
      let fn = aux unsafe in
      let safe', unsafe' = PSet.fold fn safe initAcc in
      if PSet.cardinal safe = PSet.cardinal safe' then
        safe', unsafe'
      else iterate safe' unsafe'
    in
    (* a function is very obviously unsafe if it performs I/O or writes to
       a global variable
    *)
    let simple_filt info =
      info.io ||
      not (PSet.is_empty info.writes_global)
    in
    let simpleUnsafe, simpleSafe =
      Tuple.map2 (fun set ->PSet.of_enum (PMap.keys set))
        (PMap.partition_by_value simple_filt globalFnMap) in
    iterate simpleSafe simpleUnsafe
