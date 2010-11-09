
open Base
open SSA


(* SingleDef is the combination of an expression 
   and index into multiple return values 
*)
type def = SingleDef of SSA.exp * int | CombineDef of ID.Set.t | FunArg
   
let rec eval_block env code = 
  List.fold_left eval_stmt env code
      
and eval_stmt env node = 
  match node.stmt with
  | Set (ids, rhs) ->
      let rhsDefs = eval_exp rhs in 
      IFDEF DEBUG THEN
        assert (List.length rhsDefs = List.length ids); 
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
          | SingleDef (e1,i), SingleDef (e2,j) -> 
            if i = j && e1 = e2 then SingleDef (e1, i) 
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
  | Values vs -> List.map (fun v -> SingleDef (Values [v], 0)) vs  
  | other -> 
      let numReturnVals = List.length expNode.exp_types in 
      List.map (fun i -> SingleDef (other, i)) (List.til numReturnVals)  

let find_block_defs block = eval_block ID.Map.empty block

let find_function_defs fundef = 
  let initEnv = 
    List.fold_left 
      (fun accEnv id -> ID.Map.add id FunArg accEnv) 
      ID.Map.empty 
      fundef.input_ids    
  in 
  eval_block initEnv fundef.body