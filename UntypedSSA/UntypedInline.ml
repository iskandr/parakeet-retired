open Base
open SSA 
open DynType 

let get_constant_function lookup = function 
  | Lam fundef -> Some fundef 
  | Var id -> lookup id
  | _ -> None       

let do_inline fundef argVals = 
  let bodyIds, _ = UntypedFindGenSet.block_gens fundef.body in 
  let allIds = fundef.input_ids @ (PSet.to_list bodyIds) in 
  let nvars = List.length allIds in  
  let freshIds = ID.gen_fresh_list nvars in
  let replaceMap = PMap.of_list (List.combine allIds freshIds) in 
  let body' = UntypedReplace.replace_block replaceMap fundef.body in
  let newInputIds = 
    List.map (fun id -> PMap.find id replaceMap) fundef.input_ids in
  let newOutputIds = 
    List.map (fun id -> PMap.find id replaceMap) fundef.output_ids in
  let inTypes = DynType.fn_input_types fundef.fun_type in 
  let argAssignments = 
    mk_set newInputIds (SSA.mk_vals_exp ~types:inTypes argVals) 
  in
  let outTypes = DynType.fn_output_types fundef.fun_type in 
  let outputValNodes = 
    List.map2 (fun id t -> SSA.mk_var ~ty:t id) newOutputIds outTypes 
  in 
  let outputExp = mk_exp ~types:outTypes (Values outputValNodes) in  
  argAssignments :: body', outputExp   
  

let rec inline_block lookup block = 
  let allStmts, allChanged = 
    List.split (List.map (inline_stmt lookup) block) in 
  let anyChanged = List.fold_left (||) false allChanged in 
  let flatStmts = List.concat allStmts in 
  flatStmts, anyChanged
    
and inline_stmt constEnv node =
  match node.stmt with
  | Set (ids, rhs) -> 
    let rhs', changed, extraStmts = inline_exp constEnv rhs in 
    extraStmts @ [{node with stmt=Set(ids, rhs')}], changed
  | Ignore rhs ->
    let rhs', changed, extraStmts = inline_exp constEnv rhs in 
    extraStmts @ [{node with stmt = Ignore rhs'}], changed  
  | If (condVal, tBlock, fBlock, ifGate) ->
    let condVal', condChanged = inline_value constEnv condVal in 
    let tBlock', tChanged = inline_block constEnv tBlock in 
    let fBlock', fChanged = inline_block constEnv fBlock in 
    let changed = condChanged || tChanged || fChanged in 
    [{node with stmt = If (condVal', tBlock', fBlock', ifGate)}], changed
  | SetIdx(id, indices, rhsVal) -> 
    let indices', indicesChanged = inline_value_list constEnv indices in 
    let rhsVal', rhsChanged = inline_value constEnv rhsVal in
    let changed = indicesChanged || rhsChanged  in  
    [{node with stmt = SetIdx(id, indices', rhsVal')}], changed  
and inline_exp lookup node =
  match node.exp with 
  | App ({value=Var id} as fn, args) -> 
      let args', argsChanged = inline_value_list lookup args in
      let noInline = {node with exp = App(fn, args')}, argsChanged, [] in 
      (
        match lookup id with 
        | None -> noInline 
        | Some fundef -> 
          (* make sure arity lines up *)
          if List.length fundef.input_ids <> List.length args then noInline
          else 
          let inlineCode, outputExp = do_inline fundef args in
          assert (outputExp.exp_types = node.exp_types); 
          {outputExp with src=node.src }, true, inlineCode
      )
   | App({value=Lam fundef} as fn, args) -> 
        let fundef', fundefChanged = inline_fundef lookup fundef in 
        let args', argsChanged = inline_value_list lookup args in
        let fn' = { fn with value= Lam fundef' } in 
        let noInline = 
          {node with exp = App(fn', args') }, fundefChanged || argsChanged, []
        in 
        if List.length fundef.input_ids <> List.length args then noInline
        else 
        let inlineCode, outputExp = do_inline fundef args in
        assert (outputExp.exp_types = node.exp_types); 
        {outputExp with src=node.src }, true, inlineCode
        
  | ArrayIndex (arr, indices) ->
      let arr', arrChanged = inline_value constEnv arr in 
      let indices', indicesChanged = inline_value_list constEnv indices in
      let node' = {node with exp = ArrayIndex(arr', indices')} in 
      let changed = arrChanged || indicesChanged in 
      node', changed, []
  | Arr vs ->   
      let vs', changed = inline_value_list lookup vs in
      {node with exp = Arr vs'}, changed, []
  | Values vs ->
      let vs', changed = inline_value_list lookup vs in 
      {node with exp = Values vs'}, changed, []
  | Cast(t, rhs) -> 
      let v', changed = inline_value constEnv v in 
      { node with exp = Cast(t, v') }, changed, [] 
and inline_value constEnv vNode = 
  let value', changed = match vNode.value with   
  | Lam fundef  -> 
     let body', changed = inline_block constEnv fundef.body in 
     Lam {fundef with body=body'}, changed 
  | const -> const, false
  in {vNode with value = value'}, changed  
and inline_value_list constEnv  = function 
  | [] -> [], false
  | v::vs ->
      let v', currChanged = inline_value constEnv v in 
      let vs', restChanged = inline_value_list constEnv vs in 
      v'::vs', currChanged || restChanged     
  

(* all this funny business about fn_lookup arises since the mapping of symbol*)
(* names to known functions might be stored in either a PMap or a Hashtbl. *)
(* To abstract over this difference the inliner takes a function which *)
(* accepts a symbol name and returns a fundef option *) 
let run_block_inliner ?(fn_lookup : (ID.t -> SSA.fundef option) option) code =
  let lookup = match fn_lookup with 
    | None -> 
        let constEnv = UntypedFindConstants.find_constants code in 
        (fun id -> 
          if PMap.mem id constEnv then 
            match PMap.find id constEnv with 
              | UntypedFindConstants.Const (Lam fundef) -> Some fundef 
              | _ -> None 
          else None)   
    | Some fn -> fn 
  in 
  let code', changed = inline_block lookup code in 
  code', changed   

let run_fundef_inliner function_hash fundef = 
  let lookup = (fun id -> 
    if Hashtbl.mem function_hash id then 
    Some (Hashtbl.find function_hash id) else None)
  in
  let body', changed = run_block_inliner ~fn_lookup:lookup fundef.body in 
  {fundef with body = body' }, changed        