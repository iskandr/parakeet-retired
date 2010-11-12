
open Base
open SSA


(* SingleDef is the combination of an expression 
   index into multiple return values, 
   number of returned values into total 
*)
type def = SingleDef of SSA.exp * int * int | CombineDef of ID.Set.t | FunArg
   
let rec eval_block env code = 
  List.fold_left eval_stmt env code
      
and eval_stmt env node = 
  match node.stmt with
  | Set (ids, rhs) ->
      let rhsDefs = eval_exp rhs in 
      IFDEF DEBUG THEN
        if not (List.length rhsDefs = List.length ids) then 
          failwith $ 
            Printf.sprintf 
              "[FindDefs] error in \"%s\", %d ids for %d expressions : %s\n" 
              (SSA.stmt_node_to_str node)
              (List.length ids)
              (List.length rhsDefs)
              (DynType.type_list_to_str rhs.exp_types)  
            ;
      ENDIF; 
      List.fold_left2 
        (fun accEnv id def -> ID.Map.add id def accEnv) env ids rhsDefs    
  | If (_, tBlock, fBlock, ifGate) -> 
      let trueEnv = eval_block env tBlock in 
      let falseEnv= eval_block trueEnv fBlock in
      let branchPairs = List.combine ifGate.true_ids ifGate.false_ids in
      let combineBranches accEnv outId (trueId, falseId) = 
        let def = 
          match ID.Map.find trueId trueEnv, ID.Map.find falseId falseEnv with
          | SingleDef (e1,i,m), SingleDef (e2,j,n) -> 
            if i = j && m=n && e1 = e2 then SingleDef (e1, i,m) 
            else CombineDef (ID.Set.of_list [trueId; falseId]) 
          | CombineDef ids1, CombineDef ids2 -> 
            CombineDef (ID.Set.union ids1 ids2)
          | CombineDef ids1, _ -> CombineDef (ID.Set.add falseId ids1)
          | _, CombineDef ids2 -> CombineDef (ID.Set.add trueId ids2)
          | _ -> CombineDef (ID.Set.of_list [trueId; falseId])
        in  ID.Map.add outId def accEnv
      in 
      List.fold_left2 combineBranches falseEnv ifGate.if_output_ids branchPairs 
  | _ -> env
 
and eval_exp expNode = 
  match expNode.exp with 
  | Values vs -> List.map (fun v -> SingleDef (Values [v], 1, 1)) vs  
  | other -> 
      let numReturnVals = List.length expNode.exp_types in 
      List.map 
        (fun i -> SingleDef (other, i+1, numReturnVals)) 
        (List.til numReturnVals)  

let find_block_defs block = eval_block ID.Map.empty block

let find_function_defs fundef = 
  let initEnv = 
    List.fold_left 
      (fun accEnv id -> ID.Map.add id FunArg accEnv) 
      ID.Map.empty 
      fundef.input_ids    
  in 
  eval_block initEnv fundef.body