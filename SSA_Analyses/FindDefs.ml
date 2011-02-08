(* pp: -parser o pa_macro.cmo *)

open Base
open SSA
open SSA_Analysis 

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
  
  let exp_default expNode =  match expNode.exp with 
    | Values vs -> List.map (fun v -> Val v.value) vs   
    | other -> 
      let numReturnVals = List.length expNode.exp_types in 
      List.map 
        (fun i -> Def (other, i+1, numReturnVals)) 
        (List.til numReturnVals)
end

   
module Env = struct 
  type env = (ID.t, DefLattice.t) Hashtbl.t
  let init fundef = 
    let env = Hashtbl.create 127 in  
    List.iter  
      (fun id -> Hashtbl.add env id DefLattice.Top) 
      fundef.input_ids
    ; 
    env         
end 
module DefAnalysis = struct
  include SSA_Analysis.MkAnalysis(Env)(DefLattice)
    
  let iterative = false 
  
  let stmt_set env stmtNode ~ids ~rhs ~rhsInfo = 
    IFDEF DEBUG THEN
      let nDefs = List.length rhsInfo in 
      let nIds = List.length ids in 
      if nDefs <> nIds then 
        failwith $ Printf.sprintf 
          "[FindDefs] error in \"%s\", %d ids for %d expressions" 
          (SSA.stmt_node_to_str stmtNode) nIds nDefs 
    ENDIF; 
    List.iter2 (Hashtbl.add env) ids rhsInfo;  
    None    
end 

module DefEval = MkEvaluator(DefAnalysis)
 
let find_defs = DefEval.eval_fundef  