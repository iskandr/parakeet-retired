open Base
open DynType 
open SSA
open UntypedFindConstants
open UntypedFindDefs 

(* when we deconstruct an if-statement, we need to turn its gate to a list of
   assignments 
*)
let rec create_assignments oldIds newIds =  
match oldIds, newIds with 
  | [], _ 
  | _, [] -> []
  | (oldId::oldRest), (newId::newRest) -> 
     let valNode = {value=Var newId; value_type = BottomT; value_src = None} in
     let expNode = 
       { exp = Values [valNode]; exp_types = [BottomT]; exp_src = None} 
     in  
     let assign = mk_set [oldId] expNode in 
     assign :: (create_assignments oldRest newRest) 

let rec simplify_block constEnv useCounts defEnv block = 
  let allStmts, allChanged = 
    List.split (List.map (simplify_stmt constEnv useCounts defEnv) block) in 
  let anyChanged = List.fold_left (||) false allChanged in 
  let flatStmts = List.concat allStmts in 
  flatStmts, anyChanged
    
and simplify_stmt constEnv useCounts defEnv node =
  let drop = [], true in   
  let return stmt changed = [{node with stmt = stmt}], changed in 
  let s_block = simplify_block constEnv useCounts defEnv in 
  let s_val = simplify_value constEnv useCounts defEnv in 
  let s_vals = simplify_value_list constEnv useCounts defEnv in 
  let s_exp = simplify_exp constEnv useCounts defEnv in 
  match node.stmt with  
  | Set ([id], rhs) ->
      let rhs', changed = s_exp rhs in return (Set([id], rhs')) changed
  | Ignore expNode ->
      let expNode', changed = s_exp expNode in
      if is_simple_exp expNode'.exp then drop
      else return  (Ignore expNode') changed 
      
  | SetIdx (id,indices,rhs) -> 
      let indices', indicesChanged = s_vals indices in 
      let rhs', rhsChanged = s_val rhs in 
      return (SetIdx(id, indices', rhs')) (indicesChanged || rhsChanged)
           
  | If (condVal, tBlock, fBlock, ifGate) ->
      let condVal', condChanged  = s_val condVal in 
      let tBlock', tChanged = s_block tBlock in 
      let fBlock', fChanged = s_block fBlock in 
      begin match condVal'.value with 
        | Num (PQNum.Bool true) ->
            let assignments =  
              create_assignments ifGate.if_output_ids ifGate.true_ids
            in 
            tBlock' @ assignments, true 
        | Num (PQNum.Bool false) -> 
            let assignments =
              create_assignments ifGate.if_output_ids ifGate.false_ids 
            in fBlock' @ assignments, true    
        | _ -> 
          let anyChanged = condChanged || tChanged || fChanged in 
          return (If(condVal, tBlock, fBlock, ifGate)) anyChanged  
     end   

and simplify_exp constEnv useCounts defEnv node =
  let return exp changed = {node with exp=exp}, changed in  
  match node.exp with
  | Values vs -> 
      let vs', changed = simplify_value_list constEnv useCounts defEnv vs in 
      return (Values vs') changed
   
  | App (fn, args) -> 
      let fn', fnChanged = simplify_value constEnv useCounts defEnv fn in 
      let args', argsChanged =
         simplify_value_list constEnv useCounts defEnv args in
      return (App(fn', args')) (fnChanged || argsChanged)
    
  | ArrayIndex (arr, indices) -> 
      let arr', arrChanged = simplify_value constEnv useCounts defEnv arr in 
      let indices', indicesChanged = 
        simplify_value_list constEnv useCounts defEnv indices in 
      let exp' = ArrayIndex(arr', indices') in 
      let changed =  arrChanged || indicesChanged in 
      return exp' changed
  | Arr vs ->  
      let vs', changed = simplify_value_list constEnv useCounts defEnv vs in
      return (Arr vs') changed 
  (*| Tuple vs ->
      let vs', changed = simplify_value_list constEnv useCounts defEnv vs in 
      return (Tuple vs') changed
   *)   
and simplify_value constEnv useCounts defEnv valNode = 
  let value', changed = match valNode.value with 
  | Var id -> 
      let nochange = Var id, false in 
      (match PMap.find id defEnv with 
        | UntypedFindDefs.SingleDef (Values [{value=Var id'}]) -> 
          (Var id'), true
        | _ -> 
            if PMap.mem id constEnv then match PMap.find id constEnv with
            | Const (Lam fundef) -> nochange  
            (* any constant but a function is fine to inline everywhere *)
            | Const v -> v, true 
            | _ -> nochange 
            else nochange 
      )  
  | Lam fundef  -> 
      (* sometimes the output variables of a lambdas are just a dummy assignment
         like xOut = y but it's invalid to overwrite xOut with y. So instead
         we check for dummy outputs and replace them directly
      *) 
      let rec rename_dummy_outputs = function 
        | id::ids -> 
          let ids', changed = rename_dummy_outputs ids in
          if PMap.mem id defEnv then 
            match PMap.find id defEnv with 
            | SingleDef (Values [{value=Var id'}]) -> id'::ids', true     
            |  _ -> id::ids', changed 
          else id::ids', changed
        | [] -> [], false
      in 
      let newOutIds, outputsChanged = rename_dummy_outputs fundef.output_ids in 
      let newBody, bodyChanged = 
        simplify_block constEnv useCounts defEnv fundef.body in 
      let changed = outputsChanged || bodyChanged in 
      Lam {fundef with body=newBody; output_ids = newOutIds}, changed    
  | const -> const, false
  in 
  {valNode with value = value'}, changed   
and simplify_value_list constEnv useCounts defEnv = function 
  | [] -> [], false
  | v::vs ->
      let v', currChanged = simplify_value constEnv useCounts defEnv v in 
      let vs', restChanged = simplify_value_list constEnv useCounts defEnv vs in 
      v'::vs', currChanged || restChanged   

(* run single pass of constant detection and propagation *) 
let simplify_block (functions : FnTable.t) block = 
  let constEnv  = UntypedFindConstants.find_constants block in
  let useCounts,_ = UntypedFindUseCounts.find_use_counts block in 
  let defEnv  = UntypedFindDefs.find_defs block in 
  simplify_block constEnv useCounts defEnv block                                                                  
