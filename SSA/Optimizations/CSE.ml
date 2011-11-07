(* pp: -parser o pa_macro.cmo *)

open Type 
open Base
open SSA 
open SSA_Helpers
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
  let finalize _ _ = NoChange 
  let dir = Forward
  
  let stmt env stmtNode =  match stmtNode.stmt with
    (* leave simple constants alone *)  
    | Set ([id], {exp=Values [{value = Num _}]}) -> NoChange 
    | Set ([id], expNode) when is_safe_exp expNode -> 
      if Hashtbl.mem env expNode.exp then ( 
        let rhsVal = Hashtbl.find env expNode.exp in
        let expNode' = 
          mk_vals_exp ?src:expNode.exp_src ~types:expNode.exp_types [rhsVal] 
        in 
        Update (mk_set [id] expNode')
      ) 
      else (Hashtbl.add env expNode.exp (Var id); NoChange)
    | _ -> NoChange    
  let phi env phiNode = NoChange 
    (* if both branches of the phi node aren't in the env, then 
       key not found exception gets thrown 
    *)(* 
    try ( 
      let left' = match phiNode.phi_left.value with 
      | Var leftId -> Hashtbl.find env leftId 
      | other -> other
      in 
      let right' = match phiNode.phi_right.value with 
      | Var rightId -> Hashtbl.find env rightId 
      | other -> other 
      in 
      if left' = right' then
        Hashtbl. 
    )
    with 
      | _ -> NoChange
     *)  
  let exp env envNode = NoChange
     
  let value env valNode = NoChange   
end

module CSE_Rewrite = SSA_Transform.Mk(CSE_Rules)

let cse _ = CSE_Rewrite.transform_fundef  