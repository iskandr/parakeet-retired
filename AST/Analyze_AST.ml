open Base 
open AST
open Prim
open Printf

let _ = Printexc.record_backtrace true 

type scope_info = { 
  volatile_local : string PSet.t; 	 
	volatile_global : string PSet.t;
	locals : string PSet.t; 
  maybe_locals : string PSet.t; 
}

let combine_scope_info s1 s2 = { 
			volatile_local = PSet.union s1.volatile_local s2.volatile_local; 
		  volatile_global = PSet.union s1.volatile_global s2.volatile_global; 
			locals = PSet.union s1.locals s2.locals;
      maybe_locals = PSet.union s1.maybe_locals s2.maybe_locals; 
}

let emptyScopeInfo = {
   volatile_local = PSet.empty; 
   volatile_global = PSet.empty; 
   locals = PSet.empty;
   maybe_locals = PSet.empty
}

let (++) = PSet.union 
let (<+>) = combine_ast_info 


(* annotates ast_info fields of all ast nodes, and returns a *)
(* set of volatile var names from the outermost scope *)
let analyze_ast ast = 
	let rec fold_block ~inFunction scopeInfo blockInfo = function  
		| [] -> scopeInfo, blockInfo 
		| node::nodes -> 
				let scopeInfo' = aux ~inFunction scopeInfo node in
				let blockInfo' = node.ast_info <+> blockInfo in  
				fold_block ~inFunction scopeInfo' blockInfo' nodes 				 
  and aux ~inFunction scopeInfo node =
    let analyze_block nodes = 
      let emptyBlockInfo = mk_ast_info () in 
      let scopeInfo', blockInfo =
        fold_block ~inFunction scopeInfo emptyBlockInfo nodes 
      in 
      node.ast_info <- blockInfo; 
      scopeInfo'          
    in      
		match node.data with  
		| Lam (ids, body) ->
       node.ast_info.is_function <- true; 
       node.ast_info.defs_local <- PSet.from_list ids; 
       if inFunction then 
         node.ast_info.nested_functions <- true;
       let fnScopeInfo = 
          { emptyScopeInfo with locals = PSet.from_list ids; } in 
        let fnScopeInfo' = aux fnScopeInfo ~inFunction:true body in
        node.ast_info <- node.ast_info <+> body.ast_info;
        { scopeInfo with volatile_global = 
            PSet.union scopeInfo.volatile_global fnScopeInfo'.volatile_global 
        }
    
    | CountLoop (a,b) 
    | WhileLoop (a,b) -> analyze_block [a;b]  
            
 		| Arr nodes  
		| Block nodes -> analyze_block nodes 
        
    | If(test, tNode, fNode) ->
				let testInfo = aux ~inFunction scopeInfo test in
				let tScopeInfo = aux ~inFunction testInfo tNode in
				let fScopeInfo = aux ~inFunction testInfo fNode in 
				node.ast_info <- test.ast_info <+> tNode.ast_info <+> fNode.ast_info;
        { volatile_local = 
            PSet.union tScopeInfo.volatile_local fScopeInfo.volatile_local;
          volatile_global = 
            PSet.union tScopeInfo.volatile_global fScopeInfo.volatile_global;
          locals = PSet.inter tScopeInfo.locals fScopeInfo.locals;
          maybe_locals = PSet.empty       
		    }
		| App (fn,args) -> 
     
        let emptyArgsInfo = mk_ast_info () in
				let scopeInfo', argsInfo = 
          fold_block ~inFunction scopeInfo emptyArgsInfo (List.rev args) in
        
        let scopeInfo'' = aux ~inFunction scopeInfo' fn in
        node.ast_info <- fn.ast_info <+> argsInfo;
        (* 
          HACK: 
          for now assume function calls don't produce functions 
        *)
        node.ast_info.is_function <- false; 
        
        
				scopeInfo'' 
				
    | SetIdx (name, indices, rhs) -> 
        let scopeInfo2 = aux ~inFunction scopeInfo rhs in 
        let emptyIdxInfo = mk_ast_info () in
        let scopeInfo3, idxInfo = 
          fold_block ~inFunction scopeInfo2 emptyIdxInfo indices in
        
        node.ast_info <- idxInfo <+> rhs.ast_info;
        if PSet.mem name scopeInfo3.locals then
          begin 
            node.ast_info.writes_local <- PSet.add name 
                                            node.ast_info.writes_local;
            { scopeInfo3 with 
              volatile_local = PSet.add name scopeInfo3.volatile_local 
            } 
          end    
        else begin 
          node.ast_info.writes_global <- PSet.add name 
                                           node.ast_info.writes_global;
          { scopeInfo3 with 
              volatile_global = PSet.add name scopeInfo3.volatile_global } 
        end
    | Var name ->
      let isLocal = PSet.mem name scopeInfo.locals in 
      (* printf "Analyze_AST: [var] %s, local? %B \n" name isLocal; *)  
       if inFunction &&  isLocal then 
        node.ast_info.reads_local <- PSet.add name node.ast_info.reads_local
      else 
        node.ast_info.reads_global <- PSet.add name node.ast_info.reads_global
      ;
      scopeInfo
                    
    | Def (name, rhs) ->      
      let scopeInfo' = aux ~inFunction scopeInfo rhs in 
      node.ast_info <- rhs.ast_info;
      begin if inFunction then 
        node.ast_info.defs_local <-  PSet.add name node.ast_info.defs_local
      else 
        node.ast_info.defs_global <- PSet.add name node.ast_info.defs_global
      end;
      if PSet.mem name scopeInfo'.locals then (  
        node.ast_info.writes_local <- PSet.add name node.ast_info.writes_local;
        { scopeInfo' with 
            volatile_local = PSet.add name scopeInfo'.volatile_local }
      ) 
			else 
        { scopeInfo' with locals = PSet.add name scopeInfo'.locals } 
    
   
    | Prim op ->
         (if not $ is_pure_op op then 
           node.ast_info.io <- true); 
         scopeInfo 

    | Sym _ 
    | Str _ 
    | Num _ 
    | Void -> scopeInfo 
	in 
  let finalScopeInfo = aux ~inFunction:false emptyScopeInfo ast in 
  PSet.union finalScopeInfo.volatile_local finalScopeInfo.volatile_global
  
let collect_defs ast = 
  let rec fold_block lastNode defs  = function 
    | [] -> lastNode, defs
    | node::nodes -> 
        let lastNode', defs' = aux defs node in
        fold_block lastNode' defs' nodes   
  and aux defs ast = match ast.data with 
   | Def (name, rhs) ->
      let lastNode, defs' = aux defs rhs in 
      lastNode, (name, lastNode)::defs'
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
          if varInfo.is_function then varInfo <+> acc else acc
        ) 
        else
        (* if the variable isn't recognized as a user-defined global, 
           check if it's defined in the standard library 
        *) 
        if InterpState.have_untyped_function StdLib.initState var then acc 
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
      if InterpState.have_untyped_function StdLib.initState name then 
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
       a global variable, or FOR NOW, writes to a local variable 
    *) 
    let simple_filt info = 
      info.io || 
      not (PSet.is_empty info.writes_global) 
      (*|| 
      not (PSet.is_empty info.writes_local)
      *)
    in
    let simpleUnsafe, simpleSafe = 
      Tuple.map2 (fun set ->PSet.of_enum (PMap.keys set)) 
        (PMap.partition_by_value simple_filt globalFnMap) in
    iterate simpleSafe simpleUnsafe 