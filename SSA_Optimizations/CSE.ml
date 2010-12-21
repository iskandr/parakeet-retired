open DynType 
open Base
open SSA 
open SSA_Transform 

(* expressions without side effects *) 
let is_safe_exp expNode = match expNode.exp with
  | PrimApp _ | App({value=Prim _}, _) | Arr _  | Values _ -> true
  | _ -> false (* assume function calls unsafe by default *)

(* this is a really weak form of CSE. To start handling control flow*)
(* splits I need to split this into an initial CSE_Analysis which iteratively*)
(* fills an environment, followed by a rewrite *) 

module CSE_Rules = struct
  type context = (exp, value) Hashtbl.t   
  let init _ = Hashtbl.create 127 
  let dir = Forward
  
  let stmt env stmtNode =  match stmtNode.stmt with 
    | Set ([id], expNode) when is_safe_exp expNode -> 
      if Hashtbl.mem env expNode.exp then ( 
        let rhsVal = Hashtbl.find env expNode.exp in
        let expNode' = 
          mk_vals_exp ?src:expNode.exp_src ~types:expNode.exp_types [rhsVal] 
        in 
        Update (SSA.mk_set [id] expNode')
      ) 
      else (Hashtbl.add env expNode.exp (Var id); NoChange)
    | _ -> NoChange    
  let exp env envNode = NoChange   
  let value env valNode = NoChange   
end

module CSE_Rewrite = SSA_Transform.MkSimpleTransform(CSE_Rules)

let cse _ = CSE_Rewrite.transform_fundef  