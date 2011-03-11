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
    | Combine defs1, Combine defs2 ->
        Combine (List.unique (defs1 @ defs2))
    | Combine defs, d
    | d, Combine defs ->
        if List.mem d defs then Combine defs else Combine (d::defs) 
    | x,y -> 
        if x = y then x else Combine [x;y] 
        
end

   
module DefEval = MkEvaluator(struct 
  type env = (ID.t, DefLattice.t) Hashtbl.t
  type value_info = DefLattice.t
  type exp_info = DefLattice.t list 
  
  let iterative = false
  let dir = Forward
  
  let init fundef = 
    let env = Hashtbl.create 127 in  
    List.iter  
      (fun id -> Hashtbl.add env id DefLattice.Top) 
      fundef.input_ids
    ; 
    env
  
  let value _ valNode = DefLattice.Val (valNode.SSA.value)
  
  let exp env expNode helpers = match expNode.exp with 
    | Values vs -> List.map (fun v -> DefLattice.Val v.value) vs   
    | other -> 
        let numReturnVals = List.length expNode.exp_types in 
        List.map 
          (fun i -> DefLattice.Def (other, i+1, numReturnVals)) 
          (List.til numReturnVals)  
  
  let phi_set env id defVal = 
    try 
      let oldVal = Hashtbl.find env id in 
      if  oldVal <> defVal then 
        (Hashtbl.replace env id (DefLattice.combine oldVal defVal); Some env) 
      else None
    with _ -> Hashtbl.replace env id defVal; Some env    
       
  let phi_merge env id leftVal rightVal =
    phi_set env id (DefLattice.combine leftVal rightVal)
     
  let stmt env stmtNode helpers = match stmtNode.stmt with 
    | Set(ids, rhs) ->
        let rhsInfo = exp env rhs helpers in  
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
   | _ -> helpers.eval_stmt env stmtNode     
end)

let find_defs f = DefEval.eval_fundef f  