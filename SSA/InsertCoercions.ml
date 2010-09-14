open SSA 
open Printf 
open DynType 

type tenv = (ID.t, DynType.t) PMap.t

(* Given a type environment and a list of annotated but untyped stmtNodes *)
(* return a list of typed statement nodes and the modified type environment*)
(* which might contain the types produced by any inserted coercions. *)
(* However, don't try to "coerce" functions since function values *)
(* are assumed to have been correctly specialized by the annotator.  *)

let rec rewrite_block tenv = function 
  | [] -> [], tenv     
  | stmtNode::rest -> 
        let stmts, tenv' = rewrite_stmt tenv stmtNode in 
        let restStmts, tenv'' = rewrite_block tenv' rest in 
        stmts@ restStmts, tenv''   
        
and rewrite_stmt tenv stmtNode = match stmtNode.stmt with 
  | Set (ids, rhs) ->
        (* along with the typed/coerced rhs' we also get a list of statements*)
        (* used for coercions and a modified type environment containing the *)
        (* types of any intermediaries which were produced *)
        let rhs', coerceStmts, tenv' = rewrite_exp tenv rhs in 
        let allStmts =  
            coerceStmts @ [{stmtNode with stmt = Set(ids, rhs')}] 
        in 
        (* further modify the type environment to include all assigned ids *)
        let tenv'' = List.fold_left2 
            (fun accEnv id t -> PMap.add id t accEnv)
            tenv' ids rhs'.exp_types
        in  
        allStmts, tenv'' 
                         
  | Ignore effect -> failwith "[specialize_block] ignore not yet implemented"  
  | SetIdx _ -> failwith "[specialize_block] setidx not yet implemented"  
  | If _ ->  failwith "[specialize_block] if not yet implemented"


and rewrite_exp tenv expNode = match expNode.exp with 
  | Value valNode -> 
     (* get back any necessary coercions *) 
      let valNode', stmts, tenv' = rewrite_value tenv valNode in
      {expNode with exp = Value valNode'}, stmts, tenv'  
  | Cast (t, valNode) -> 
      let valNode', stmts, tenv' = rewrite_value tenv valNode in
      {expNode with exp = Cast(t, valNode')}, stmts, tenv'

  | App (fn, args) -> 
    (* Don't try to rewrite the function value, since this is assumed to *)
    (* already have been changed to a typed specialized function by the *)
    (* annotator.*)  
      let args', argsStmts, tenv' = rewrite_values tenv args in 
      { expNode with exp = App (fn, args') }, argsStmts, tenv' 
  
  | ArrayIndex (array, indices) -> 
      let array', arrayStmts, tenv' = rewrite_value tenv array in 
      let indices', indicesStmts, tenv'' = rewrite_values tenv' indices in 
      let expNode' = { expNode with exp = ArrayIndex (array', indices') } in 
      expNode', arrayStmts @ indicesStmts, tenv'' 
    
  | Arr valNodes -> 
      let valNodes', stmts, tenv' = rewrite_values tenv valNodes in 
      { expNode with exp = Arr valNodes' }, stmts, tenv' 
  

(* pass the newer contexts forward through recursion and concatenate the*)
(* block of statements as your return from each nesting level *)    
and rewrite_values tenv = function 
  | [] -> [], [], tenv 
  | v::vs -> 
      let v', stmts, tenv' = rewrite_value tenv v in 
      let vs', stmtsRest, tenvRest = rewrite_values tenv' vs in 
      v'::vs', stmts @ stmtsRest, tenvRest 
         
and rewrite_value tenv valNode =
  (* if no coercions necessary, return this value *) 
  let nochange = valNode, [], tenv in
  let expect_type t = 
    if valNode.value_type <> t then 
        failwith
            (sprintf "no coercions available from %s to %s"
                (DynType.to_str t) 
                (DynType.to_str valNode.value_type))
            
    else nochange
  in   
  match valNode.value with 
  | Var id ->  
      let actualType = PMap.find id tenv in 
      let annotatedType = valNode.value_type in 
      if actualType <> annotatedType then
        let coerceExp = {
          exp=Cast(annotatedType, valNode); 
          exp_src = valNode.value_src; 
          exp_types = [annotatedType]
        } 
        in   
        let id' =  ID.gen() in 
        let stmts = [mk_set ~src:valNode.value_src [id'] coerceExp] in 
        (  
          if List.length coerceExp.exp_types <> 1 then 
            failwith "coercion can't change arity of expression"
          else
            let coerceType = List.hd coerceExp.exp_types in 
            let valNode' = { 
              value=Var id'; 
              value_type = coerceType;
              value_src = valNode.value_src
            } 
            in valNode', stmts, PMap.add id' coerceType tenv  
        )   
        else nochange 
  | Num n ->
      let n' = PQNum.coerce_num n valNode.value_type in 
      let valNode' =  {valNode with value = Num n' } in 
      valNode', [], tenv  
  | Str _ -> expect_type StrT  
  | Sym _ ->  expect_type SymT 
  | Unit -> expect_type UnitT 
  | Prim p -> 
     failwith "[insert_coercions::rewrite_value] unexpected primitive"
  | Lam fundef -> 
     failwith "[insert_coercions::rewrite_value] unexpected anonymous function" 
     
