open Base
open SSA 
open DynType 


module Inline_Rules = struct 
  type env = (DynType.t list) option   
  let init _ = ref []  
  let dir = Forward 
  
  let stmt env stmtNode = None       
  let exp env expNode = match expNode with 
    | App ({value=GlobalFn fnId} as fn, args) -> 
      (match lookup fnId with 
        | None -> NoChange  
        | Some fundef -> 
          (* make sure arity lines up *)
          if List.length fundef.input_ids <> List.length args then NoChange
          else 
          let inlineCode, outputExp, typesList = do_inline fundef args in
          IFDEF DEBUG THEN 
            assert (outputExp.exp_types = node.exp_types);
          ENDIF;  
          let expNode' = {outputExp with exp_src=node.exp_src } in
           something with typesList??    
          UpdateWithStmts(expNode', inlineCode)
       )
    | _ -> NoChange 
  
  let value env valNode = NoChange       
end 

let run_fundef_inliner (functions : FnTable.t) fundef = 
  let lookup = fun id -> FnTable.find_option id functions in 
  let body', typesList, changed = 
    run_block_inliner ~fn_lookup:lookup fundef.body   
  in 
  let tenv' = 
    List.fold_left 
      (fun accEnv (id,t) -> ID.Map.add id t accEnv) 
      fundef.tenv 
      typesList  
  in 
  {fundef with body = body'; tenv = tenv' }, changed    

let get_constant_function lookup = function 
  | Lam fundef -> Some fundef 
  | GlobalFn id -> lookup id
  | _ -> None       


(* create fresh names for all variables in a body, 
   replace names everywhere,
   bind input ids to given argument expressions,  
   return list of id, type pairs to augment 
   the type environment of whichever function this code
   will be spliced into 
*)

let do_inline fundef argVals = 
  let bodyIds, _ = UntypedFindGenSet.block_gens fundef.body in 
  let allIds = fundef.input_ids @ (ID.Set.to_list bodyIds) in 
  let nvars = List.length allIds in  
  let freshIds = ID.gen_fresh_list nvars in
  let replaceMap = ID.Map.of_list (List.combine allIds freshIds) in 
  let body' = Replace.replace_block replaceMap fundef.body in
  let newInputIds = 
    List.map (fun id -> ID.Map.find id replaceMap) fundef.input_ids in
  let newOutputIds = 
    List.map (fun id -> ID.Map.find id replaceMap) fundef.output_ids in
  let inTypes = 
    if fundef.fundef_type = DynType.BottomT then 
      List.map (fun _ -> DynType.BottomT) newInputIds
    else  DynType.fn_input_types fundef.fundef_type 
  in
  let argAssignments = 
    mk_set newInputIds (SSA.mk_exp ~types:inTypes (Values argVals)) 
  in
  let outTypes =
    if fundef.fundef_type = DynType.BottomT then 
      List.map (fun _ -> DynType.BottomT) newOutputIds 
    else DynType.fn_output_types fundef.fundef_type 
  in 
  let outputValNodes = 
    List.map2 (fun id t -> SSA.mk_var ~ty:t id) newOutputIds outTypes 
  in 
  let outputExp = mk_exp ~types:outTypes (Values outputValNodes) in
  (* list of new ids and their types-- ignore types missing from tenv *) 
  let types = 
    List.fold_left2 
      (fun accList id id' ->
          if ID.Map.mem id fundef.tenv then 
            (id', ID.Map.find id fundef.tenv)::accList
          else accList
      )
      []
      allIds
      freshIds 
  in 
  argAssignments :: body', outputExp, types   
  

let rec inline_block lookup block = 
  let allStmts, allTypesLists, allChanged  = 
    List.split3 (List.map (inline_stmt lookup) block) in 
  let anyChanged = List.fold_left (||) false allChanged in 
  let flatStmts = List.concat allStmts in 
  let flatTypesList = List.concat allTypesLists in 
  flatStmts, flatTypesList, anyChanged

and inline_fundef lookup fundef = 
  let body', types, changed  = inline_block lookup fundef.body in
  if changed then 
    let tenv' = 
      List.fold_left 
        (fun accEnv (id,t) -> ID.Map.add id t accEnv) 
        fundef.tenv 
        types
    in 
    { fundef with body = body'; tenv = tenv'} , changed
  else fundef, false  
       
and inline_stmt constEnv node =
  match node.stmt with
  | Set (ids, rhs) -> 
    let rhs', changed, extraStmts, extraTypes = inline_exp constEnv rhs in 
    extraStmts @ [{node with stmt=Set(ids, rhs')}], extraTypes, changed
  | If (condVal, tBlock, fBlock, ifGate) ->
    let condVal', condChanged = inline_value constEnv condVal in 
    let tBlock', tTypes, tChanged = inline_block constEnv tBlock in 
    let fBlock', fTypes, fChanged = inline_block constEnv fBlock in 
    let changed = condChanged || tChanged || fChanged in
    let types = tTypes @ fTypes in  
    [{node with stmt = If (condVal', tBlock', fBlock', ifGate)}], types, changed
  | SetIdx(id, indices, rhsVal) -> 
    let indices', indicesChanged = inline_value_list constEnv indices in 
    let rhsVal', rhsChanged = inline_value constEnv rhsVal in
    let changed = indicesChanged || rhsChanged  in  
    [{node with stmt = SetIdx(id, indices', rhsVal')}], [], changed
      
and inline_exp lookup node =
  let nochange = node, false, [], [] in 
  let return exp = { node with exp = exp}, true, [], [] in   
  match node.exp with 
  | App ({value=GlobalFn id} as fn, argNodes) -> 
      let argNodes', argsChanged = inline_value_list lookup argNodes in
      let noInline = 
        {node with exp = App(fn, argNodes')}, argsChanged, [], [] 
      in 
      (
        match lookup id with 
        | None -> noInline 
        | Some fundef -> 
          (* make sure arity lines up *)
          if List.length fundef.input_ids <> List.length argNodes' then noInline
          else 
          let inlineCode, outputExp, typesList = do_inline fundef argNodes' in
          assert (outputExp.exp_types = node.exp_types); 
          {outputExp with exp_src=node.exp_src }, true, inlineCode, typesList
      )
  | App({value=Lam fundef} as fn, args) -> 
      let fundef', fundefChanged = inline_fundef lookup fundef in 
      let args', argsChanged = inline_value_list lookup args in
      let fn' = { fn with value= Lam fundef' } in 
      let noInline = 
        {node with exp = App(fn', args') }, fundefChanged || argsChanged, [], []
      in 
      if List.length fundef.input_ids <> List.length args then noInline
      else 
      let inlineCode, outputExp, typesList = do_inline fundef args' in
      assert (outputExp.exp_types = node.exp_types); 
      {outputExp with exp_src=node.exp_src }, true, inlineCode, typesList
  | App(fn, args) -> 
      let args', argsChanged = inline_value_list lookup args in
      if argsChanged then return (App(fn, args'))
      else nochange  
  | ArrayIndex (arr, indices) ->
      let arr', arrChanged = inline_value lookup arr in 
      let indices', indicesChanged = inline_value_list lookup indices in
      let changed = arrChanged || indicesChanged in
      if changed then return (ArrayIndex(arr', indices'))
      else nochange  
  | Arr vs ->   
      let vs', changed = inline_value_list lookup vs in
      if changed then return (Arr vs') else nochange  
  | Values vs ->
      let vs', changed = inline_value_list lookup vs in
      if changed then return (Values vs') else nochange  
  | Cast(t, rhs) -> 
      let rhs', changed = inline_value lookup rhs in 
      if changed then return (Cast(t,rhs')) else nochange 
and inline_value lookup vNode = 
  let nochange = vNode, false in 
  let return v = {vNode with value = v}, true in 
  match vNode.value with   
  | Lam fundef  -> 
    let fundef', changed = inline_fundef lookup fundef in
    if changed then return (Lam fundef')
    else nochange  
  | const -> nochange   
and inline_value_list lookup  = function 
  | [] -> [], false
  | v::vs ->
      let v', currChanged = inline_value lookup v in 
      let vs', restChanged = inline_value_list lookup vs in 
      v'::vs', currChanged || restChanged     

let run_fundef_inliner (functions : FnTable.t) fundef = 
  let lookup = fun id -> FnTable.find_option id functions in 
  let body', typesList, changed = 
    run_block_inliner ~fn_lookup:lookup fundef.body   
  in 
  let tenv' = 
    List.fold_left 
      (fun accEnv (id,t) -> ID.Map.add id t accEnv) 
      fundef.tenv 
      typesList  
  in 
  {fundef with body = body'; tenv = tenv' }, changed        