open Base 
open SSA



let rec eval_block useMap block =
  let rec aux accLive accMap = function 
    | [] -> accLive, accMap 
    | stmt::rest -> 
        let set, map = eval_stmt accMap stmt in 
        aux (ID.Set.union accLive set) map rest 
  in   
  aux ID.Set.empty useMap block 
and eval_stmt useMap stmtNode = 
  match stmtNode.stmt with  
  | Set (ids, expNode) -> 
      let currUsedSet, useMap' = eval_exp useMap expNode in
      let useMap''' = List.fold_left 
        (fun accMap id -> PMap.add id currUsedSet accMap)
        useMap'     
        ids
      in 
      currUsedSet, useMap'''  
       
  | Ignore expNode -> eval_exp useMap expNode 
  | SetIdx (id, indices, rhsVal) -> 
      let oldSet = PMap.find id useMap in 
      let idxSet, map' = eval_value_list useMap indices in
      let rhsSet, map'' = eval_value map' rhsVal in
      let combinedSet = ID.Set.union oldSet (ID.Set.union idxSet rhsSet) in 
      combinedSet, PMap.add id combinedSet map''   
          
  | If (cond, tBlock, fBlock, ifGate) -> 
      let condSet, condMap = eval_value  useMap cond in 
      let trueSet, trueMap = eval_block condMap tBlock in 
      let falseSet, falseMap = eval_block trueMap fBlock in
      (* for each ID leaving the if-gate, it uses its input value from 
         each branch of the conditional 
      *)  
      let combineBranches accEnv outId (trueId, falseId) = 
        PMap.add outId (ID.Set.of_list [trueId; falseId]) accEnv
      in 
      let branchIds = List.combine ifGate.true_ids ifGate.false_ids in 
      let outIds = ifGate.if_output_ids in 
      let gateMap = List.fold_left2 combineBranches falseMap outIds branchIds in
      ID.Set.union condSet (ID.Set.union trueSet falseSet), gateMap       
and eval_exp useMap expNode = 
  match expNode.exp with 
  | Values vs -> eval_value_list useMap vs    

  | ArrayIndex (lhs, args)   
  | App (lhs, args) ->
      let (set1, map1) = eval_value useMap lhs in  
      let (set2, map2) = eval_value_list map1 args in
      ID.Set.union set1 set2, map2 
  | Arr vs -> failwith "not implemented" 
    
and eval_value useMap valNode =
  match valNode.value with 
  | Lam fundef ->
      (* remember to add the output variables of the function to the 
         liveSet or else they get pruned...and then so does the rest of 
         the function 
      *) 
      let liveSet, useMap = eval_block useMap fundef.body in 
      let liveSet' = 
        ID.Set.union liveSet (ID.Set.of_list fundef.output_ids) 
      in 
      liveSet', useMap 
  | Var id -> ID.Set.singleton id, useMap 
  | _ -> ID.Set.empty, useMap 
and eval_value_list useMap = function 
  | [] -> ID.Set.empty, useMap 
  | v::vs -> 
      let currSet, currMap = eval_value useMap v in 
      let restSet, restMap = eval_value_list currMap vs in
      (ID.Set.union currSet restSet), restMap 

(* a naive application of FindUses to the entire program would consider 
   any function/variable which isn't called as unused, so 
   we gather the top level definitions and add them to the live set 
*) 
let rec find_top_bindings = function 
  | [] -> ID.Set.empty 
  | {stmt=Set([id],_)}::rest-> ID.Set.add id (find_top_bindings rest)
  | {stmt=If (_, tBlock, fBlock, ifGate)}::rest ->
      let tSet = find_top_bindings tBlock in 
      let fSet = find_top_bindings fBlock in 
      let gateSet = ID.Set.of_list ifGate.if_output_ids in 
      ID.Set.union gateSet (ID.Set.union tSet fSet)  
  | _::rest -> find_top_bindings rest
 
let find_use_sets block = 
  eval_block PMap.empty block 