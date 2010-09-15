open Base
include QSyntax
include AST

let rec syntax_to_ast  (syntax, src) = 
		let mk node = mk_node ~src node  in 
		let gen_conditional = function 
		| [e1;e2;e3] -> If(e1,e2,e3)
		| _ -> let msg = "complicated if statements not yet implemented" in 
				 raise (SourcedError (msg, src))
    in let exp = match syntax with                                 
    | BlockExp nodes ->  Block (List.map syntax_to_ast nodes)
    | DefExp (name,rhs) -> Def(name, syntax_to_ast rhs)
    | SetIdxExp (id, indices, rhs) -> 
        SetIdx (id, List.map syntax_to_ast indices, syntax_to_ast rhs) 
    | Id name -> QPrims.prim_or_var name
    | FloatLit f -> Num (PQNum.Float64 f) 
    | RealLit r ->  Num (PQNum.Float32 r)
    | IntLit i ->  Num (PQNum.Int32 i)
    | ShortLit s -> Num (PQNum.Int s)
    | LongLit l -> Num (PQNum.Int64 l) 
    | BitLit b -> Num (PQNum.Bool (Bool.of_int b))
    | StrLit s -> if String.length s = 1 then Num (PQNum.Char s.[0]) else Str s
    | SymLit s -> Sym s
    | AppExp (fn, args) -> 
        let args'  = List.map syntax_to_ast args in
        let nargs = List.length args' in 
        (* the overloaded use of $ is dumb *) 
        if fst fn = Id "$" && nargs > 2  then gen_conditional args'
        (* because Q is written by developmentally challenged hyperactive 
           children, the ? operator generates rands with 2 arguments and
           performs a vector conditional for 3. Indicate this
           to prim_or_var_name with ??
        *)   
        else if fst fn = Id "?" && nargs > 2 then  
          App (syntax_to_ast (Id "??", snd fn), args')
        else App (syntax_to_ast fn, args')
    | LamExp (args, body) -> Lam (args, syntax_to_ast body) 
    | SimpleLamExp body -> 
        let body' = syntax_to_ast body in 
        let vars = collect_vars body' String.Set.empty in
        let args = 
            if String.Set.mem "z" vars then ["x";"y";"z"]
            else if String.Set.mem "y" vars then ["x";"y"]
            else if String.Set.mem "x" vars then ["x"]
            else []
        in Lam(args, body')
    | ArrExp elts -> Arr (List.map syntax_to_ast elts) 
    | TimeExp stx -> 
        let tic = mk $ Prim (Prim.ImpureOp Prim.ResetTimer) in 
        let toc = mk $ Prim (Prim.ImpureOp Prim.GetTimer) in
        let node = syntax_to_ast stx in   
        Block [mk $ App(tic, []); node; mk $ App(toc, [])]
    | ControlExp(id, args) ->
	    let args' = List.map syntax_to_ast args in 
		  begin match id with 
		  | "$" -> gen_conditional args'   
		  | "if" -> 
        begin match args' with 
			  | condition::rest -> 
			    let void = mk Void  in 
          let block = mk (Block rest) in If(condition, block, void)
        | _ -> raise (SourcedError("too few arguments for if statement", src))
        end
      | "do" ->
        begin match args' with
          (* generate all the necessary code for a for-loop *)
			  | upper_limit::code -> 
			    let block = mk (Block code) in CountLoop (upper_limit, block)
			  | _ -> 
				  let msg = "insufficient arguments for do-loop" in 
            raise (SourcedError(msg, src))
        end
      | "while" ->
         begin match args' with 
			    | condition::code -> 
			      let block = mk (Block code) in WhileLoop (condition, block)
			    | _ -> 
            raise (SourcedError ("insufficient arguments for while-loop",src)) 
		     end
        | _ -> raise (SourcedError("unknown control structure", src)) 	
      end 
    in mk exp
and remove_list ids set = 
    List.fold_left (fun set id -> String.Set.remove id set) set ids  
and collect_arg_vars args set = 
    List.fold_left (fun acc_set node -> collect_vars node acc_set) set args
and collect_vars exp set = match exp.data with 
    | Lam (ids, body) -> 
        let body_vars = collect_vars body String.Set.empty in 
        let body_vars' = remove_list ids body_vars in 
        String.Set.union set body_vars' 
    | Var name -> String.Set.add name set
    | App(fn,args) -> 
        let set' = collect_vars fn set in 
        collect_arg_vars args set'   
    | Def (name, rhs) ->  
        let set' = String.Set.add name set in 
        collect_vars rhs set' 
    | Arr nodes  
    | Block nodes -> collect_arg_vars nodes set   
    | SetIdx(name,indices,rhs) -> 
        let set' = String.Set.add name set in 
        let set'' = collect_arg_vars indices set' in 
        collect_vars rhs set'' 
    | If (cond, trueCode, falseCode) -> 
			let set' = collect_vars cond set in 
			let set'' = collect_vars trueCode set' in 
			collect_vars falseCode set'' 
    | WhileLoop (_, code) -> collect_vars code set
    | _  -> set 


		
     
