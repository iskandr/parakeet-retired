open SSA 
open SSA_Transform
open Printf 
open DynType 

(* Given a type environment and a list of annotated but untyped stmtNodes *)
(* return a list of typed statement nodes and the modified type environment*)
(* which might contain the types produced by any inserted coercions. *)
(* However, don't try to "coerce" functions since function values *)
(* are assumed to have been correctly specialized by the annotator.  *)

module type REWRITE_PARAMS = sig
  include TypeAnalysis.TYPE_ANALYSIS_PARAMS 
  val tenv : (ID.t, DynType.t) Hashtbl.t 
end  
 
module Rewrite_Rules (P: REWRITE_PARAMS) = struct 
  (* since the SSA_Transform framework only allows for bottom-up transformations
     we need to specify these coerce_* functions for type information 
     to flow from bindings down to values 
  *) 
  let coerce_value valNode t = 
    if valNode.value_type = t then valNode, [] 
    else 
      let coerceExp = SSA.mk_cast localType valNode in    
      let id' =  ID.gen() in 
      let valNode' = SSA.mk_var ~ty:localType id' in
      Hashtbl.add P.tenv id' localType;
      valNode', [SSA.mk_set [id'] coerceExp]  
  
  let rec coerce_values valNodes ts = match valNodes, ts with 
    | valNode::restNodes, t::restTypes -> 
        let valNode', stmts = coerce_value valNode t in 
        let valNodes', stmtsRest = coerce_values restNodes restType in 
        valNode'::valNodes', stmts @ stmtsRest  
    | [], [] -> [], []
    | _ -> assert false 

  let coerce_exp expNode ts = 
    let exp', stmts = match expNode.exp, ts with 
      | SSA.Arr eltNodes, [DynType.VecT eltT] ->
        let types = List.map (fun _ -> eltT) eltNodes in  
        let eltNodes', eltStmts = coerce_values eltNodes types in 
        SSA.Arr eltNodes', eltStmts 
          
      | SSA.Values eltNodes, _ -> 
        let eltNodes', eltStmts = coerce_values eltNodes ts in 
        SSA.Values eltNodes', eltStmts 
      | _ -> failwith "not implemented"
    in
    {expNode with exp = exp'; exp_types = ts }   
  
end 
     
class rewriter initTypeEnv = object
    inherit SSA_Transform.default_transformation 
    val mutable tenv : DynType.t ID.Map.t = initTypeEnv
    method value valNode =
      let localType = valNode.value_type in 
      match valNode.value with 
      | Var id -> 
        let globalType = ID.Map.find id tenv in
        if globalType <>  localType then 
          
  
          )
        else SSA_Transform.NoChange
      | Num n -> 
          let n' = PQNum.coerce_num n localType in
          if n <> n' then SSA_Transform.Update (SSA.mk_num ~ty:localType n')
          else NoChange 
      | _ -> NoChange 
     
    method get_tenv = tenv 
end

let rewrite_block tenv block =
  let r = new rewriter tenv in 
  let block', _ = 
    SSA_Transform.transform_block (r :> SSA_Transform.transformation) block 
  in
  block', r#get_tenv  