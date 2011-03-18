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
  type env =  DefLattice.t ID.Map.t 
  type value_info = DefLattice.t
  type exp_info = DefLattice.t list 
  
  let iterative = true
  let dir = Forward
  
  let init fundef = 
    List.fold_left 
      (fun env id -> ID.Map.add id DefLattice.Top env)
      ID.Map.empty 
      fundef.input_ids
    
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
      let oldVal = ID.Map.find id env in 
      if  oldVal <> defVal then 
        let combined = DefLattice.combine oldVal defVal in 
        Some (ID.Map.add id combined env) 
      else None
    with _ -> Some (ID.Map.add id defVal env)    
       
  let phi_merge env id leftVal rightVal =
    phi_set env id (DefLattice.combine leftVal rightVal)
     
  let stmt env stmtNode helpers = match stmtNode.stmt with 
    | Set(ids, rhs) ->
        let rhsDefs = exp env rhs helpers in  
        IFDEF DEBUG THEN
          let nDefs = List.length rhsDefs in 
          let nIds = List.length ids in 
          if nDefs <> nIds then 
            failwith $ Printf.sprintf 
              "[FindDefs] error in \"%s\", %d ids for %d expressions" 
              (SSA.stmt_node_to_str stmtNode) nIds nDefs 
        ENDIF; 
        let changed = ref false in
        let update_def (env:env) (id:ID.t) (newDef:DefLattice.t) = 
          if ID.Map.mem id env then 
            let oldDef = ID.Map.find id env in (
            if oldDef <> newDef then (
              changed := true;
              let combined = DefLattice.combine oldDef newDef in 
              ID.Map.add id combined env
            )
            else env
          ) 
          else ( 
            changed := true; 
            ID.Map.add id newDef env
          )
        in  
        let env' = List.fold_left2 update_def env ids rhsDefs in 
        if !changed then Some env' else None 
         
   | _ -> helpers.eval_stmt env stmtNode     
end)

let find_defs f = DefEval.eval_fundef f  