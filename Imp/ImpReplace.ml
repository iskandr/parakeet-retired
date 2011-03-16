open Base
open Imp 
open Printf 
  

(* FIND/REPLACE expressions in Imp expression *)
let rec apply_exp_map (eMap:Imp.exp_node ID.Map.t)  expNode = 
  let aux = apply_exp_map eMap in 
  match expNode.exp with  
  | Var id -> if ID.Map.mem id eMap then ID.Map.find id eMap else expNode
  | Idx (e1,e2) -> {expNode with exp = Idx(aux e1, aux e2) }  
  | Op (op, argT, es) -> 
      { expNode with exp = Op (op, argT, List.map aux es) } 
  | Select (t, e1, e2, e3) -> 
      { expNode with exp = Select(t, aux e1, aux e2, aux e3) }   
  | Cast (t1, e) -> 
      { expNode with exp = Cast (t1, aux e) }   
  | DimSize (n, e) -> { expNode with exp = DimSize (n, aux e) }
  | _ -> expNode

(* FIND/REPLACE expressions in Imp statement *)
let rec apply_exp_map_to_stmt (eMap :Imp.exp_node ID.Map.t) stmt =
  let aux_stmt = apply_exp_map_to_stmt eMap in
  let aux_exp = apply_exp_map eMap in  
  match stmt with  
  | If (cond,tBlock,fBlock) -> 
      If(aux_exp cond, List.map aux_stmt tBlock, List.map aux_stmt fBlock)
  | While (cond,body) -> While(aux_exp cond, List.map aux_stmt body)
  | Set (id, rhs) -> 
       let rhs' = aux_exp rhs in 
       if ID.Map.mem id eMap then
         let expNode' = ID.Map.find id eMap in
         let exp' = expNode'. exp in   
         match exp' with 
         | Var id' ->  Set(id', rhs')
         | Idx _ -> 
            let arrayId, indices = Imp.collect_indices exp' in 
            SetIdx (arrayId, indices, rhs')
         | other -> 
            failwith $ sprintf 
              "[apply_exp_map] cannot replace variable with: %s" 
              (Imp.exp_to_str exp'); 
       else Set(id, rhs')
  | SetIdx (id,indices,rhs) ->
       let rhs' = aux_exp rhs in 
       let indices' = List.map aux_exp indices in  
        if ID.Map.mem id eMap then 
          let expNode' = ID.Map.find id eMap in  
          match expNode'.exp with 
          | Var id' -> SetIdx(id', indices', rhs')
          | _ ->  SetIdx(id, indices', rhs')
        else SetIdx(id, indices', rhs') 
  | other -> other

(* FIND/REPLACE identifiers in Imp expression *)  
let rec apply_id_map idMap expNode = 
  let aux = apply_id_map idMap in 
  let exp' = match expNode.exp with  
  | Var id -> if ID.Map.mem id idMap then Var (ID.Map.find id idMap) else Var id
  | Idx (e1,e2) -> Idx(aux e1, aux e2)  
  | Op (op, argT, es) -> Op (op, argT, List.map aux es) 
  | Select (t, e1, e2, e3) -> Select(t, aux e1, aux e2, aux e3)   
  | Cast (t1, e) -> Cast (t1, aux e)   
  | DimSize (n, e) -> DimSize (n, aux e)
  | other -> other
  in {expNode with exp = exp'} 

(* FIND/REPLACE identifiers in Imp statement *) 
let rec apply_id_map_to_stmt idMap stmt =
  let aux_stmt = apply_id_map_to_stmt idMap in
  let aux_exp = apply_id_map idMap in  
  match stmt with  
  | If (cond,tBlock,fBlock) -> 
      If(aux_exp cond, List.map aux_stmt tBlock, List.map aux_stmt fBlock)
  | While (cond,body) -> While(aux_exp cond, List.map aux_stmt body)
  | Set (id, rhs) -> 
       let rhs' = aux_exp rhs in 
       if ID.Map.mem id idMap then  Set(ID.Map.find id idMap, rhs')
       else Set(id, rhs')
      
  | SetIdx (id,indices,rhs) ->
       let rhs' =  aux_exp rhs in 
       let indices' = List.map  aux_exp indices in
        if ID.Map.mem id idMap then  SetIdx (ID.Map.find id idMap, indices', rhs') 
        else SetIdx(id, indices', rhs')
  | other -> other
