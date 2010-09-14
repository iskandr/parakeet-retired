
open Base
open SSA


type def = SingleDef of SSA.exp | CombineDef of ID.t PSet.t | FunArg
   
let rec eval_block env code = 
  List.fold_left eval_stmt env code
      
and eval_stmt env node = 
  match node.stmt with
  | Set ([id], rhs) -> PMap.add id (SingleDef rhs.exp) (eval_exp env rhs)  
  | If (_, tBlock, fBlock, ifGate) -> 
      let trueEnv = eval_block env tBlock in 
      let falseEnv= eval_block trueEnv fBlock in
      let branchPairs = List.combine ifGate.true_ids ifGate.false_ids in
      let combineBranches accEnv outId (trueId, falseId) = 
        let def = 
          match PMap.find trueId trueEnv, PMap.find falseId falseEnv with
          | SingleDef e1, SingleDef e2 -> 
            if e1 = e2 then SingleDef e1 
            else CombineDef (PSet.of_list [trueId; falseId]) 
          | CombineDef ids1, CombineDef ids2 -> 
            CombineDef (PSet.union ids1 ids2)
          | CombineDef ids1, _ -> CombineDef (PSet.add falseId ids1)
          | _, CombineDef ids2 -> CombineDef (PSet.add trueId ids2)
          | _ -> CombineDef (PSet.of_list [trueId; falseId])
        in  PMap.add outId def accEnv
      in 
      List.fold_left2 combineBranches falseEnv ifGate.if_output_ids branchPairs 
  | _ -> env
(* doesn't yet work correctly with multiple values *) 
and eval_exp env expNode = 
  match expNode.exp with 
  | Values [{value=Lam fundef}] -> 
     let env' = List.fold_left 
       (fun accEnv id -> PMap.add id FunArg accEnv) env fundef.input_ids
     in eval_block env' fundef.body 
  | _ -> env     
let find_defs block = eval_block PMap.empty block 

 