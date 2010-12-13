(* pp: -parser o pa_macro.cmo *)

open Base
open SSA
open SSA_Analysis 

(* SingleDef is the combination of an expression 
   index into multiple return values, 
   number of returned values into total 
*)
module DefLattice = struct
  type t = 
    | Val of SSA.value  
    | Def of SSA.exp * int * int 
    | Combine of t list 
    | Top 
    | Bottom

  let bottom = Bottom
   
  let combine x y = match x,y with
    | _, Top 
    | Top, _ -> Top 
    | Bottom, x 
    | x, Bottom -> x
    | Val n1, Val n2 -> 
      if n1 = n2 then x else Combine [x;y] 
    | Def (e1,i,m), Def (e2,j,n) -> 
        if i = j && m=n && e1 = e2 then x else Combine [x;y]
    | Combine defs1, Combine defs2 -> Combine (defs1 @ defs2)
    | Combine defs, d
    | d, Combine defs -> Combine (d::defs)
    
  let eq = (=)  
end
module Env = struct 
       
end 
module DefAnalysis = struct 
  type env = (ID.t, DefLattice.t) Hashtbl.t 
  type exp_info = DefLattice.t list
  type value_info = unit 
    
  let init fundef =
    let env = Hashtbl.create 127 in  
    List.iter  
      (fun id -> Hashtbl.add env id DefLattice.Top) 
      fundef.input_ids
    ; 
    env
    
  let dir = Forward 
  let flow_split env = env, env
  let flow_merge = SSA_Analysis.mk_hash_merge DefLattice.combine
    
  let iterative = false 
  
  let value _ _ = () 
  let exp env expNode _ =  match expNode.exp  with 
    | Values vs -> List.map (fun v -> DefLattice.Val v.value) vs   
    | other -> 
      let numReturnVals = List.length expNode.exp_types in 
      List.map 
        (fun i -> DefLattice.Def (other, i+1, numReturnVals)) 
        (List.til numReturnVals)  
  
  let stmt env stmtNode stmtInfo = match stmtNode.stmt, stmtInfo with
    | Set (ids, _), SetInfo defs -> 
      IFDEF DEBUG THEN
        if not (List.length defs = List.length ids) then 
          failwith $ Printf.sprintf 
            "[FindDefs] error in \"%s\", %d ids for %d expressions" 
            (SSA.stmt_node_to_str stmtNode) (List.length ids) (List.length defs)
      ENDIF; 
      List.iter2 (Hashtbl.add env) ids defs; 
      None   
    | _ -> None 
end 

module DefEval = MkEvaluator(DefAnalysis)
 
let find_defs = DefEval.eval_fundef  