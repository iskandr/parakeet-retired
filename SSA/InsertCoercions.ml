open SSA 
open Printf 
open DynType 

type tenv = DynType.t ID.Map.t

class rewriter initTypeEnv = object
    val mutable tenv : ID.Map.t = initTypeEnv
    method set ids rhs = (* do something with tenv *) NoChange
    method if_ cond tBlock fBlock gate =  (* do something with tenv *) NoChange
    
    
    (*let expect_type t = 
    if valNode.value_type <> t then 
        failwith
            (sprintf "no coercions available from %s to %s"
                (DynType.to_str t) 
                (DynType.to_str valNode.value_type))
            
    else nochange
  in   
  *)
    method value valNode = 
      let ty = valNode.value_type in 
      match valNode.value with 
      | Var id -> 
        let actualType = ID.Map.find id tenv in 
        if actualType <> ty then
          let coerceExp = SSA.mk_cast ty valNode in    
          let id' =  ID.gen() in 
          let bindings = [[id'], coerceExp] in 
          let valNode' = SSA.mk_var ~ty:coerceType id' in
          (tenv <- ID.Map.add id' coerceType tenv; 
           SSA_Transform.UpdateWithBindings(valNode', bindings)
          )
        else SSA_Transform.NoChange
      | Num n -> 
          let n' = PQNum.coerce_num n ty in
          if n <> n' then SSA_Transform.Update (SSA.mk_num ~ty n')
          else NoChange 
    


    method str valNode _ -> expect_type valNode StrT  
    method valNode _ ->  expect_type valNode SymT 
  | Unit -> expect_type valNode UnitT 
  | Prim _ -> 
     failwith "[InsertCoercions->rewrite_value] unexpected primitive"
  | GlobalFn _ -> 
     failwith "[InsertCoercions->rewrite_value] unexpected global function" 
  | Lam _ -> 
     failwith "[InsertCoercions->rewrite_value] unexpected anonymous function" 

      
        
            

(* Given a type environment and a list of annotated but untyped stmtNodes *)
(* return a list of typed statement nodes and the modified type environment*)
(* which might contain the types produced by any inserted coercions. *)
(* However, don't try to "coerce" functions since function values *)
(* are assumed to have been correctly specialized by the annotator.  *)

     
