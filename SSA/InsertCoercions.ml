open SSA 
open Printf 
open DynType 

type tenv =  DynType.t ID.Map.t 

      
(* Given a type environment and a list of annotated but untyped stmtNodes *)
(* return a list of typed statement nodes and the modified type environment*)
(* which might contain the types produced by any inserted coercions. *)
(* However, don't try to "coerce" functions since function values *)
(* are assumed to have been correctly specialized by the annotator.  *)

     
class rewriter initTypeEnv = object
    inherit SSA_Transform.default_transformation 
    val mutable tenv : DynType.t ID.Map.t = initTypeEnv
    method value valNode =
      match valNode.value with 
      | Var id -> 
        let globalType = ID.Map.find id tenv in
        let localType = valNode.value_type in  
        if globalType <>  localType then 
          let coerceExp = SSA.mk_cast localType valNode in    
          let id' =  ID.gen() in 
          let bindings = [[id'], coerceExp] in 
          let valNode' = SSA.mk_var ~ty:localType id' in
          (tenv <- ID.Map.add id' localType tenv; 
           SSA_Transform.UpdateWithBindings(valNode', bindings)
          )
        else SSA_Transform.NoChange
      | Num n -> 
          let n' = PQNum.coerce_num n ty in
          if n <> n' then SSA_Transform.Update (SSA.mk_num ~ty n')
          else NoChange 
      | _ -> NoChange 
     
    method get_tenv = tenv 
end

let rewrite_block tenv block =
  let r = new rewriter tenv in 
  let block', _ = 
    SSA_Transform.transform_block (r :> SSA_Transform.transformation) block 
  in
  let tenv' = r#get_tenv in 
  block', tenv'  
   