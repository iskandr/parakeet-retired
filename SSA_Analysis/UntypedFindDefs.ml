
open Base
open SSA


type def = SingleDef of SSA.exp | CombineDef of ID.Set.t | FunArg
   
let rec eval_block env code = 
  List.fold_left eval_stmt env code
      
and eval_stmt env node = 
  match node.stmt with
  | Set ([id], rhs) -> ID.Map.add id (SingleDef rhs.exp) (eval_exp env rhs)  
  | If (_, tBlock, fBlock, ifGate) -> 
      let trueEnv = eval_block env tBlock in 
      let falseEnv= eval_block trueEnv fBlock in
      let branchPairs = List.combine ifGate.true_ids ifGate.false_ids in
      let combineBranches accEnv outId (trueId, falseId) = 
        let def = 
          match ID.Map.find trueId trueEnv, ID.Map.find falseId falseEnv with
          | SingleDef e1, SingleDef e2 -> 
            if e1 = e2 then SingleDef e1 
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
(* doesn't yet work correctly with multiple values *) 
and eval_exp env expNode = 
  match expNode.exp with 
  | Values [{value=Lam fundef}] -> 
     let env' = List.fold_left 
       (fun accEnv id -> ID.Map.add id FunArg accEnv) env fundef.input_ids
     in eval_block env' fundef.body 
  | _ -> env     
let find_defs block = eval_block ID.Map.empty block 

 