open Printf 
open Base
open SourceInfo
open AST

let _ = Printexc.record_backtrace true

let gen_template_id = mk_gen () 
let gen_template_name () = "pq_template" ^ Int.to_string (gen_template_id())

let rename_global x = "pq_global_" ^ x

let old_fn_name fnName = "pq_old_" ^ fnName 
let moduleTemplateName = "pq_module_template"


(* These two functions "prepend_old" and "fold_prepend_old" rewrite the names
   of referenced variables such that any function names start with "pq_old_"
*)
let rec fold_prepend_old (knownFunctions : String.Set.t) ?(accNodes =[]) = 
  function 
    | [] -> List.rev accNodes, knownFunctions 
    | node::rest -> 
        let node', knownFunctions' = prepend_old knownFunctions node in 
        fold_prepend_old knownFunctions' ~accNodes:(node'::accNodes) rest 

and prepend_old (knownFunctions : String.Set.t) ast = match ast.data with 
  | Var x -> 
    if String.Set.mem x knownFunctions then 
      { ast with data = Var (old_fn_name x) }, knownFunctions
    else ast, knownFunctions 
  | Def (name, rhs) ->
    let rhs', knownFunctions' = prepend_old knownFunctions rhs in
    (* if we're redefining a known function, assume rhs is a non-function 
       value and remove name from the set 
    *)
    let knownFunctions'' = String.Set.remove name knownFunctions' in 
    { ast with data = Def(name, rhs') }, knownFunctions''
  | Block nodes -> 
    let nodes', knownFunctions' = fold_prepend_old knownFunctions nodes in 
    { ast with data = Block nodes' }, knownFunctions'    
  | Lam(ids, body) ->
    let knownFunctions' = 
      List.fold_left 
        (fun knownFns name -> String.Set.remove name knownFns)
        knownFunctions
        ids 
    in 
    let body', _ = prepend_old knownFunctions' body in 
    { ast with data = Lam (ids, body')}, knownFunctions 
         
  | App(fn, args) ->
    let args', knownFunctions' = fold_prepend_old knownFunctions args in 
    let fn', knownFunctions'' = prepend_old knownFunctions' fn in 
    { ast with data = App(fn', args') }, knownFunctions''    
  | SetIdx(lhs, indices, rhs) ->
    let rhs', knownFunctions' = prepend_old knownFunctions rhs in 
    let indices', knownFunctions'' = 
      fold_prepend_old knownFunctions' indices
    in 
    { ast with data = SetIdx(lhs, indices', rhs') }, knownFunctions'
  | Arr nodes -> 
    let nodes', knownFunctions' = fold_prepend_old knownFunctions nodes in 
    { ast with data = Arr nodes' }, knownFunctions'   
  | _ -> ast, knownFunctions  (* TODO: if, loops *) 
  
(* 
   rewrite function calls to known functions to also send 
   the global variables of those functions. 
   Also, rewrite access to a global to prepend "_global_".  
*) 

let get_global x globalDataMap = 
  if PMap.mem x globalDataMap then PMap.find x globalDataMap 
  else failwith $ Printf.sprintf "%s not found in global data map" x

let rewrite_global_reads 
      (globalDataVars : string PSet.t) 
      (globalDataMap : (string,string PSet.t) PMap.t)
      ast =
  (* map every function name to a list of global arguments, renamed to be 
     distinctively global 
  *)
  let rec fold_block locals revNewNodes oldNodes = match oldNodes with 
    | [] -> List.rev revNewNodes, locals
    | node::nodes -> 
      let ast', locals' = self locals node in 
      fold_block locals' (ast'::revNewNodes) nodes 
   and  self locals ast =  
     match ast.data with 
    | Var x ->
        let ast' = 
          if PSet.mem x locals then ast
          else if PSet.mem x globalDataVars then 
            update_var_node ast (rename_global x)
          else if PMap.mem x globalDataMap then (  
            let globals = PMap.find x globalDataMap in 
            let globalsRenamed = 
              List.map rename_global (PSet.to_list globals)  
            in
            let globalVars = List.map mk_var_node globalsRenamed in
            (* what do we do with globalVars? *)   
            ast
         )
         (* if the variable is neither local nor globally defined, 
            is it in the standard library? 
          *)
         else if InterpState.have_untyped_function QStdLib.initState x then
            ast
         else failwith $ 
            Printf.sprintf "Couldn't find global variable %s on %s"
            x
            (SourceInfo.to_str ast.src)  
        in ast', locals
    | Def (name, rhs) -> 
        let rhs', locals' = self locals rhs in 
        let ast' = update_def_node ast name rhs' in 
        ast', PSet.add name locals'
    | Block nodes -> 
        let nodes', locals' = fold_block locals [] nodes in 
        update_block_node ast nodes', locals'
    | Lam(ids, body) -> 
        let body', _ = self (PSet.from_list ids) body in 
        let ast' = update_lam_node ast ids body' in
        ast', locals 
    | App(fn, args) -> 
        let args',locals' = fold_block locals [] args in 
        let fn',locals'' = self locals' fn in
        let ast' = update_app_node  ast fn' args' in 
        ast', locals'' 
                
    | _ -> ast, locals
 in let ast', _ = self PSet.empty ast in ast'

(* 
    given a map from function names -> ast nodes,
    and a list of entries for gen_module_template,
    resort the entries so that all of a function's dependencies
    are defined before it
*)

let topsort_function_entries fnMap entries = 
  let fnNames = List.map (fun (name,_,_,_)->name) entries  in
  let getFnGlobals ast = 
    let globals = ast.ast_info.reads_global in
    PSet.inter globals (PSet.from_list fnNames)
  in 
  (* map each function name to the other functions it calls *)
  let callsMap = PMap.map getFnGlobals fnMap in
  let orderedFns = PGraph.topsort_labels callsMap in 
  let fold_entries acc  (name,locals,globals,bodyText)  = 
    PMap.add name (name,locals,globals,bodyText) acc  
  in 
  let entryMap = List.fold_left fold_entries PMap.empty entries   in   
  List.map (fun name -> PMap.find name entryMap) orderedFns   

(* this is an ugly hack to get around Q's annoying habit of 
   treating 1-element strings and characters as equivalent. 
   Unless we put a NULL at the end of the args, we get 
   ("x"; "y") --> "xy"
*)
let add_void_terminator = function 
  | [] -> []
  | nodes -> nodes @ [mk_void_node ()] 

(* generate AST to call pq_run_template *) 
let gen_run_node fnName argNames templateName globalVars = 
  let argNodes = add_void_terminator $ List.map mk_var_node argNames in
  let globalVarNodes = add_void_terminator $ List.map mk_var_node globalVars in   
  let retName = "pqvalue" in 
  let retVar = mk_var_node retName in
  let appNode = mk_app_node (mk_var_node "pq_run_template") $
    [
      mk_var_node templateName;
      mk_arr_node argNodes; 
      mk_arr_node globalVarNodes;
    ] 
  in 
  (* the value returned by pq_run_template stores an error code in 
     its first array index and either a value or error message in 
     its second array index 
  *) 
  let firstElt = mk_idx_node retVar 0 in 
  let secondElt = mk_idx_node retVar 1 in 
  (* did the computation succeed on the GPU? *) 
  let successCheck = mk_eq_node firstElt (mk_int_node 0) in 
  let printErr = mk_app_node (mk_var_node "pq_report_error") [secondElt] in
  (* did the GPU give up on the computation? *) 
  let gaveupCheck = mk_eq_node firstElt (mk_int_node 1) in 
  let useOldFn = mk_app_node (mk_var_node (old_fn_name fnName)) argNodes in
  let ifNode = 
    mk_if_node successCheck secondElt 
      (mk_if_node gaveupCheck useOldFn printErr) in 
          
  let blockNode = mk_block_node [mk_def_node retName appNode; ifNode] in   
  mk_def_node fnName (mk_lam_node argNames blockNode)
  
let gen_module_entry_node  name locals globals bodyStr =
let localNodes = add_void_terminator $ List.map mk_str_node locals in 
  let globalNodes = add_void_terminator $ List.map mk_str_node globals in    
  mk_arr_node [
    mk_str_node name;
    mk_arr_node localNodes; 
    mk_arr_node globalNodes;  
    mk_str_node bodyStr
  ]

let reportErrNode = 
  mk_def_node "pq_report_error" 
    (mk_lam_node ["msg"] $
      mk_block_node 
      [
        mk_app_node (mk_int_node 2) 
        [
          mk_concat_node (mk_str_node "PQ ERROR: ")
          (mk_concat_node (mk_var_node "msg") (mk_str_node "\\n"))
        ];
                
        mk_app_node (mk_var_node "exit") [mk_int_node 1]
      ]
    ) 

let gen_module_node moduleTemplateName orderedEntryNodes =  
   mk_def_node moduleTemplateName $ 
     mk_app_node 
      (mk_var_node "pq_gen_module_template") 
      [mk_arr_node orderedEntryNodes]
           

 let gen_fn_template_node name fnTemplateName = 
    mk_def_node fnTemplateName 
      (mk_app_node (mk_var_node "pq_get_function_template")
        [ mk_var_node moduleTemplateName; mk_str_node name ] 
       )

(* used to escape a string, now just kills new lines *) 
let str_escape str = 
  Str.global_replace (Str.regexp_string "\n") "" str

(* rewrite safe functions to use gen_template/run_template. 
   The reason we have a map of safe function names -> ast nodes
   is so that we guarantee the RHS is always a lambda node 
   in cases like f:g:{x}
*)
let rec rewrite_ast safeFnMap dataReadsMap ast = 
  let rec fold_block moduleDefs fnTemplates revNodes = function 
    | [] ->  List.rev revNodes, moduleDefs, fnTemplates
    | node::nodes -> 
        let node', moduleDefs', fnTemplates' = 
            aux moduleDefs fnTemplates node in 
        let revNodes' = node'::revNodes in 
        fold_block moduleDefs' fnTemplates' revNodes' nodes
  
  and aux moduleEntries fnTemplates ast = 
    let return ast = ast, moduleEntries, fnTemplates in 
    match ast.data with 
    | Def(fnName, rhsOriginal) -> 
      if PMap.mem fnName safeFnMap then 
        (* process any definitions that may be on rhs *) 
        let rhsOriginal', moduleEntries', fnTemplates' =
          aux moduleEntries fnTemplates rhsOriginal 
        in
        (* have to rename all function variable names to start with pq_old_
           so that we call Q code and not into our interpreter
        *)
        let fnNameSet = 
          List.fold_left 
            (fun accSet (name, _) -> String.Set.add name accSet)
            String.Set.empty 
            fnTemplates 
        in     
        let rhsOldNames, _ = prepend_old fnNameSet rhsOriginal' in  
        let ast' = mk_def_node (old_fn_name fnName) rhsOldNames in
        (* use the rhs from the safeFnMap instead of rhsOriginal
           since the map has already pulled out lambda nodes from 
           code like f:g:h:{x+y}
        *)
        let rhs = PMap.find fnName safeFnMap in  
        let rhsStr = str_escape $ node_to_str (get_lam_body rhs) in
        let args =  get_lam_args rhs in
        let gen_arg_id = mk_gen ()  in 
        let gen_arg_name () = "pqarg" ^  Int.to_string (gen_arg_id()) in 
        let argsRenamed = List.map (fun s -> gen_arg_name() ^ "_" ^ s) args in      
        let globals = PSet.to_list $ PMap.find fnName dataReadsMap in
        let templateName = gen_template_name () in 
        let fnTemplates'' = (fnName,templateName)::fnTemplates' in 
        
        let globalsRenamed = List.map rename_global globals in 
        let moduleEntry = fnName,args,globalsRenamed,rhsStr in 
        let moduleEntries'' = moduleEntry::moduleEntries' in 
        let runNode = gen_run_node fnName argsRenamed templateName globals in
        let ast'' = mk_block_node [ast'; runNode] in
        ast'', moduleEntries'', fnTemplates''  
     else return ast
    | Block nodes -> 
        let nodes', moduleEntries', fnTemplates' = 
            fold_block moduleEntries fnTemplates [] nodes in 
        update_block_node ast nodes', moduleEntries', fnTemplates'   
    | _ -> return ast 
  in
  
  let ast', moduleEntries, fnTemplates = aux [] [] ast in 
  match moduleEntries with 
    | [] ->  ast'
    | _ ->  
      let orderedEntries = topsort_function_entries safeFnMap moduleEntries in
      let orderedEntryNodes = 
        List.map (Tuple.uncurry4 gen_module_entry_node) orderedEntries in 
     
      let moduleNode = gen_module_node moduleTemplateName orderedEntryNodes in 
      let fnTemplateNodes = 
         List.map 
         (fun (name,fnTemplateName) -> gen_fn_template_node name fnTemplateName) 
         fnTemplates in  
      mk_block_node (reportErrNode::moduleNode::fnTemplateNodes @ [ast']) 

let process_lexbuf ~debug lexbuf = 
  (* parse lexbuf into a Q-specific syntax tree *) 
		let syntax_node = 
      try 
        QParser.program  QLexer.token lexbuf 
      with 
      | QParser.Error ->
        let pos = Lexing.lexeme_start_p lexbuf in  
        eprintf "Parser error at line %d, column %d.\n" 
          pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
        ; 
        exit 1
     | QLexer.Error msg -> 
        let pos = Lexing.lexeme_start_p lexbuf in  
        eprintf "Lexer error at line %d, column %d: %s\n" 
          pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1) msg
        ; 
        exit 1 
    in 
    (* convert the syntax tree into a language agnostic AST *)
  	let ast = QSyntax_to_AST.syntax_to_ast syntax_node in
    
    (* annotates AST and returns set of volatile var global var names *) 
		let volatileSet = Analyze_AST.analyze_ast ast in

    let defsList = Analyze_AST.collect_defs ast in
    let astMap = PMap.of_list defsList in 
    let infoMap = PMap.map (fun  ast -> ast.ast_info) astMap in
     
    let print_info key info = 
        eprintf "%s : %s\n"  key (AST_Info.to_str info)
    in
    IFDEF DEBUG THEN 
      eprintf "[direct AST info]\n";  
      PMap.iter print_info infoMap;
      eprintf "\n"; 
    ENDIF;
    let transInfo = 
      Analyze_AST.transitive_closure infoMap 
    in
    IFDEF DEBUG THEN 
      eprintf "[transitive closure of AST info]\n";
      PMap.iter print_info transInfo;
      eprintf "\n";
    ENDIF; 
    
    (* we want the transitive properties except for is_function, which should
       only apply if the function was clearly a lambda 
    *)
    let globalFnMap, globalDataMap  =
      PMap.partition_by_key 
        (fun x -> 
            if PMap.mem x infoMap then (PMap.find x infoMap).is_function
            else failwith $ Printf.sprintf "no info about global variable %s" x    
        ) 
        infoMap in
        
    let globalFnSet = PSet.of_enum (PMap.keys globalFnMap) in 
    let globalDataSet = PSet.of_enum (PMap.keys globalDataMap) in 
    
    IFDEF DEBUG THEN 
       eprintf "Global data: {%s} \n"
        (String.concat ", " (PSet.to_list globalDataSet)); 
    ENDIF; 
    let volatileFnSet = PSet.inter volatileSet globalFnSet in         
    let safeFns, unsafeFns = 
      Analyze_AST.find_safe_functions globalFnMap volatileFnSet in  
    
    IFDEF DEBUG THEN  
        eprintf "Safe functions: {%s}\n" 
          (String.concat ", " (PSet.to_list safeFns));   
    ENDIF;
    
    let safeFnDefs = 
      PMap.filter_by_key (fun k -> PSet.mem k safeFns) astMap in  
    let globalReadsMap = 
      PMap.map (fun info -> info.reads_global) transInfo 
    in
    
    let globalDataReadsMap =
      PMap.map
       (fun set -> 
          (* HACK: exclude library functions from global reads--
             This is wrong if the user actually defines some global
             data with the same name as a library function 
          *) 
          PSet.filter 
            (fun name -> not $ 
              InterpState.have_untyped_function QStdLib.initState name)
          (PSet.inter globalDataSet set)
       ) 
       globalReadsMap
    in
    
    (* rename all the global variables used in safe functions so they never 
       conflict with locals 
    *) 
       
    let safeFnDefs' =
       PMap.map 
        (rewrite_global_reads globalDataSet globalDataReadsMap)
        safeFnDefs 
    in  
    let ast' = 
      flatten_block $
        rewrite_ast safeFnDefs' globalDataReadsMap ast in 
    let astStr = AST.node_to_str ast' in
    (* temporary: load the dt.so shared library *) 
    let astStr = "\\l dt.q\n" ^ astStr in 
    print_endline astStr
    
let process_file debug filename = 
  let channel = open_in filename in 
  let lexbuf = Lexing.from_channel channel in 
  process_lexbuf ~debug lexbuf 
     
                    
let rec prompt () = assert false 
(*
	print_string "> ";
	let str = read_line () in
	if str = "\\\\" then ()
	else
    (* create a lexing stream from the input string *)
		let lexbuf = Lexing.from_string str in
    process_lexbuf ~debug:true lexbuf;  
    prompt ()
  *)
let _ = 
    (* if a file argument was given, then load that filename
       otherwise enter an interactive prompt 
    *) 
    if Array.length Sys.argv > 1 then
       let filename, debug = 
         if Array.length Sys.argv = 2 then Sys.argv.(1), false
         else Sys.argv.(2), Sys.argv.(1) = "-debug"  
       in  
       process_file debug filename  
    else prompt ()
  