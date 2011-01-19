open Base
open DynType 
open SSA
open SSA_Transform
open FindUseCounts
open FindDefs

 
let is_useless useCounts id =
  (* by convention, having 0 use counts excludes a varialbe from the 
     useCount map, but check just in case 
  *) 
  if Hashtbl.mem useCounts id  
  then Hashtbl.find useCounts id = 0 
  else true  

module SimplifyRules = struct
  let dir = Forward
  (* use counts-- constant map? *) 
  type context = {
    constants: SSA.value ConstantLattice.t ID.Map.t;
    defs : (ID.t, DefLattice.t) Hashtbl.t;  
    use_counts : (ID.t, int) Hashtbl.t; 
    types : DynType.t ID.Map.t; 
  } 
      
  let init fundef = {
    constants = FindConstants.find_constants fundef;
    defs = FindDefs.find_defs fundef;  
    use_counts = FindUseCounts.find_fundef_use_counts fundef;
    types = fundef.tenv;   
  } 
     
  let finalize cxt fundef = NoChange 
  let stmt cxt stmtNode = match stmtNode.stmt with 
    | Set (ids, rhs) when List.for_all (is_useless cxt.use_counts) ids -> 
      Update SSA.empty_stmt
    | If (condVal, tBlock, fBlock, ifGate) ->
      let get_type id = ID.Map.find id cxt.types in
      let mk_var id t  = SSA.mk_var ?src:stmtNode.stmt_src ~ty:t id in  
      begin match condVal.value with 
        | Num (PQNum.Bool b) ->
            let branchIds = if b then ifGate.true_ids else ifGate.false_ids in 
            let types = List.map get_type branchIds in 
            let rhsVals = List.map2 mk_var branchIds types in   
            let rhsExp = 
              SSA.mk_exp ?src:stmtNode.stmt_src ~types (Values rhsVals) 
            in
            let assignment = 
              SSA.mk_set ?src:stmtNode.stmt_src ifGate.if_output_ids rhsExp
            in     
            Update assignment
        | _ -> NoChange  
      end   
    | _ -> NoChange 
    
  
  let exp cxt expNode = NoChange 
  
  let value cxt valNode = match valNode.value with
    | Var id -> 
      begin match ID.Map.find_option id cxt.constants with 
        | Some ConstantLattice.Const v -> Update {valNode with value = v }
        | Some _ 
        | None -> 
          (match Hashtbl.find cxt.defs id with 
            | FindDefs.DefLattice.Val v -> Update {valNode with value = v }
            | _ -> NoChange
          )   
      end
    | _ -> NoChange  
end

module Simplifer = SSA_Transform.MkSimpleTransform(SimplifyRules)

let simplify_fundef (_ : FnTable.t) = Simplifer.transform_fundef 
  

(* TODO: remake this module using SSA_Transform *)
(* open Base

(* chain together function which return a changed boolean *) 
let (>>) (value,changed) f = 
  if changed then (value,changed) else (Lazy.force_val f)

let replace_with_def defEnv id = 
  if ID.Map.mem id defEnv then match ID.Map.find id defEnv with 
    | FindDefs.SingleDef (Values [{value=Var id'}], _, _) ->  Var id', true
    | _ -> Var id, false
  else Var id, false 

let replace_with_const constEnv id = 
  let nochange = Var id, false in 
  if ID.Map.mem id constEnv then match ID.Map.find id constEnv with
    | ConstantLattice.Const (Lam fundef) -> nochange  
    (* any constant but a function is fine to inline everywhere *)
    | ConstantLattice.Const v -> v, true
    | _ -> nochange
  else nochange  

(* when we deconstruct an if-statement, we need to turn its gate to a list of
   assignments 
*)


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
  (* 
    TODO: make this work when only some variables in an assignment are useless 
  *) 
  | Set (ids, rhs) when List.for_all (is_useless useCounts) ids -> [], true 
  | Set (ids, rhs) ->
      let rhs', changed = s_exp rhs in return (Set(ids, rhs')) changed
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
      (match v'.value with 
        | Num n -> 
          let coercedNum = {v' with value = Num (PQNum.coerce_num n t)} in  
          return (Values [coercedNum]) true 
        | _ ->    
          if v'.value_type = t then return (Values [v']) true 
          else return (Cast(t,v')) changed
      ) 

    
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
          if ID.Map.mem id defEnv then 
            match ID.Map.find id defEnv with 
            | SingleDef (Values [{value=Var id'}], _, _) -> id'::ids', true     
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
    (functions : FnTable.t)
    ~tenv
    ~def_env 
    ~use_counts
    ~(free_vars:ID.t list) 
     (block : SSA.block) =
  let constEnv  = FindConstants.find_constants ~free_vars block in
  rewrite_block constEnv use_counts def_env tenv block     

(* sometimes we find ourselves in the silly situation such that 
     x1 = some complex expression  
     x2 = x1
   and our current optimizations are too weak to remove the extra assignment.
   We eliminate some obvious cases by rewriting 
     x2 = some complex expression 
   when (1) x1 has a unique known definition, (2) x1 is only used once (by x2)
  
   TODO: generalize for multi-assignments, allow for statements other than set.
   WARNING: This optimization will duplicate work unless it is immediately
   followed by a simplification step which will clean up useless assignments.   
*)  

let copy_redundant_stmt stmtNode defEnv useCounts = 
  let nochange = stmtNode, false, defEnv in 
  match stmtNode.stmt with 
  | Set([id], expNode) ->
     (match expNode.exp with 
      | Values [{value=Var rhsId}] 
         when Hashtbl.mem useCounts rhsId && Hashtbl.find useCounts rhsId = 1 ->
          (match ID.Map.find rhsId defEnv with
          | SingleDef (exp, 1, 1) as def->
             Hashtbl.replace useCounts rhsId 0; 
             {stmtNode with stmt = Set([id], { expNode with exp = exp})}, 
             true,  
             ID.Map.remove rhsId (ID.Map.add id def defEnv)
          | _ -> nochange
          ) 
     | _ -> nochange
     )    
  | _ -> nochange  

let rec copy_redundant_block  
        ?(changed=false) 
        ?(accBody=[]) 
        defEnv 
        (useCounts : (ID.t, int) Hashtbl.t) = 
  function
  | [] -> List.rev accBody, changed,  defEnv 
  | stmt::rest -> 
      let stmt', changed', defEnv'  = copy_redundant_stmt stmt defEnv useCounts 
      in   
      copy_redundant_block 
        ~changed:(changed||changed') 
        ~accBody:(stmt'::accBody)
        defEnv'
        useCounts 
        rest
  
    
let simplify_fundef (functions:FnTable.t) fundef =
  let defEnv =  FindDefs.find_function_defs fundef in
  let useCounts = FindUseCounts.find_fundef_use_counts fundef in
  (* forward propagate expressions through linear use chains...
     will create redundant work unless followed by a simplification 
  *)
  let body1, changed1, defEnv1 = 
    copy_redundant_block defEnv useCounts fundef.body
  in
  (* perform simple constant propagation and useless assignment elim *)
  let body2, changed2 = 
    simplify_typed_block functions 
      ~tenv:fundef.tenv 
      ~def_env:defEnv1
      ~use_counts:useCounts   
      ~free_vars:fundef.input_ids  
      body1
  in 
  {fundef with body = body2}, changed1 || changed2
*)                                                            