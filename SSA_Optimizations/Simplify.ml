open Base
open DynType 
open SSA
open UntypedFindDefs 

(* chain together function which return a changed boolean *) 
let (>>) (value,changed) f = 
  if changed then (value,changed) else (Lazy.force_val f)
  

let replace_with_def defEnv id = 
  if PMap.mem id defEnv then match PMap.find id defEnv with 
    | UntypedFindDefs.SingleDef (Values [{value=Var id'}]) ->  Var id', true
    | _ -> Var id, false
  else Var id, false 

let replace_with_const constEnv id = 
  let nochange = Var id, false in 
  if ID.Map.mem id constEnv then match ID.Map.find id constEnv with
    | ConstantLattice.Const (Lam fundef) -> nochange  
    (* any constant but a function is fine to inline everywhere *)
    | ConstantLattice.Const v ->
               debug $ Printf.sprintf 
                 "[simplify->rewrite_value found constant value for %s: %s"
                 (ID.to_str id)
                 (SSA.value_to_str v)
               ;
               v, true
    | _ -> nochange
  else nochange  

(* when we deconstruct an if-statement, we need to turn its gate to a list of
   assignments 
*)
let rec create_assignments tenv newIds oldIds =  
  match newIds, oldIds with 
  | [], [] -> []
  | [], _ -> 
    failwith "[create_assignments] expected id lists to be of same length" 
  | (newId::newRest), (oldId::oldRest) ->
     let ty = PMap.find_default oldId tenv DynType.BottomT in  
     let valNode = SSA.mk_var ~ty:ty oldId in 
     let expNode = SSA.mk_exp ~types:[ty] (Values [valNode]) in 
     let assign = mk_set [newId] expNode in 
     assign :: (create_assignments tenv newRest oldRest) 

let rec rewrite_block constEnv useCounts defEnv tenv block = 
  let allStmts, allChanged = 
    List.split (List.map (rewrite_stmt constEnv useCounts defEnv tenv) block) in 
  let anyChanged = List.fold_left (||) false allChanged in 
  let flatStmts = List.concat allStmts in 
  flatStmts, anyChanged
    
and rewrite_stmt constEnv useCounts defEnv tenv node =
  let drop = [], true in   
  let return stmt changed = [{node with stmt = stmt}], changed in 
  let s_block = rewrite_block constEnv useCounts defEnv tenv in 
  let s_val = rewrite_value constEnv useCounts defEnv tenv in 
  let s_vals = rewrite_value_list constEnv useCounts defEnv tenv in 
  let s_exp = rewrite_exp constEnv useCounts defEnv tenv in 
  match node.stmt with  
  | Set (ids, rhs) ->
      let rhs', changed = s_exp rhs in return (Set(ids, rhs')) changed
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
              create_assignments tenv ifGate.if_output_ids ifGate.true_ids
            in 
            tBlock' @ assignments, true 
        | Num (PQNum.Bool false) -> 
            let assignments =
              create_assignments tenv ifGate.if_output_ids ifGate.false_ids 
            in fBlock' @ assignments, true    
        | _ -> 
          let anyChanged = condChanged || tChanged || fChanged in 
          return (If(condVal, tBlock, fBlock, ifGate)) anyChanged  
     end   

and rewrite_exp constEnv useCounts defEnv tenv node =
  let return exp changed = {node with exp=exp}, changed in  
  match node.exp with
  | Values vs -> 
      let vs', changed = rewrite_value_list constEnv useCounts defEnv tenv vs in 
      return (Values vs') changed
   
  | App (fn, args) -> 
      let fn', fnChanged = rewrite_value constEnv useCounts defEnv tenv fn in 
      let args', argsChanged =
         rewrite_value_list constEnv useCounts defEnv tenv args in
      return (App(fn', args')) (fnChanged || argsChanged)
    
  | ArrayIndex (arr, indices) -> 
      let arr', arrChanged = rewrite_value constEnv useCounts defEnv tenv arr in 
      let indices', indicesChanged = 
        rewrite_value_list constEnv useCounts defEnv tenv indices in 
      let exp' = ArrayIndex(arr', indices') in 
      let changed =  arrChanged || indicesChanged in 
      return exp' changed
  | Arr vs ->  
      let vs', changed = rewrite_value_list constEnv useCounts defEnv tenv vs in
      return (Arr vs') changed 
  
  | Cast(t, v) ->
      let v', changed = rewrite_value constEnv useCounts defEnv tenv v in 
      if v.value_type = t then return (Values [v]) true 
      else return (Cast(t,v')) changed 

    
and rewrite_value constEnv useCounts defEnv tenv valNode =
  let value', changed = match valNode.value with 
  | Var id -> 
      (replace_with_def defEnv id) >>  lazy (replace_with_const constEnv id)
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
        rewrite_block constEnv useCounts defEnv tenv fundef.body in 
      let changed = outputsChanged || bodyChanged in 
      Lam {fundef with body=newBody; output_ids = newOutIds}, changed    
  | const -> const, false
  in 
  {valNode with value = value'}, changed   
and rewrite_value_list constEnv useCounts defEnv tenv = function 
  | [] -> [], false
  | v::vs ->
      let v', currChanged = rewrite_value constEnv useCounts defEnv tenv v in 
      let vs', restChanged = 
        rewrite_value_list constEnv useCounts defEnv tenv vs 
      in 
      v'::vs', currChanged || restChanged   

(* run single pass of constant detection and propagation *) 
let simplify_typed_block 
    ~(tenv:(ID.t, DynType.t) PMap.t)
    ~(free_vars:ID.t list) 
     (functions : FnTable.t)
     (block : SSA.block) =
  let constEnv  = FindConstants.find_constants ~free_vars block in
  
  let useCounts,_ = UntypedFindUseCounts.find_use_counts block in
  
  let defEnv  = UntypedFindDefs.find_defs block in 
  rewrite_block constEnv useCounts defEnv tenv block     
  
let simplify_untyped_block = simplify_typed_block ~tenv:PMap.empty ~free_vars:[]
  
let simplify_fundef (functions:FnTable.t) fundef = 
  
  let body', changed = 
    simplify_typed_block 
      ~tenv:fundef.tenv 
      ~free_vars:fundef.input_ids  
      functions 
      fundef.body
  in 
  let fundef' = {fundef with body = body' } in 
  fundef', changed 
                                                                                                                                                                                       
