open Base
open SSA

(* CONSTANTS AREN'T TYPED YET!! *) 
(*
let rec create_assignments ids vals =  
match ids, vals with 
  | [], _ 
  | _, [] -> []
  | (id::ids), (value::values) ->
    {stmt=Set ([id], Value value); stmt_src=None}::
    (create_assignments ids values) 
    
let rec get_constant_value constEnv vNode = match vNode.value with  
  | Var id when PMap.mem id constEnv -> 
      (match PMap.find id constEnv with 
        | ConstantLattice.Const (Var id) ->  
            get_constant_value constEnv (Var id)
        | ConstantLattice.Const v -> Some v 
        | _ -> None
       )
   | Num n -> Some (Num n)
   | _ -> None    


(* used for List.fold_left2 over args and identifiers *) 
let rec partition_constant_args constEnv ids args types =
  match (ids, args, types) with  
  | [],_,_
  | _,[],_
  | _,_,[] -> ([],[])
  | id::ids, arg::args, ty::types ->  
     let const, residual = partition_constant_args constEnv ids args types in 
     match get_constant_value constEnv arg with 
     | Some (Num n) -> (id,Num n,ty)::const, residual
     | _ ->  const, (id,arg,ty)::residual

 

let specialize_function constEnv fundef args = 
  let constTriples, keptTriples = 
    partition_constant_args constEnv fundef.input_ids args fundef.input_types in  
  if constTriples = [] then None
  else   
  let constIds, constVals, constTypes = List.split3 constTriples in
  
  let keptIds, keptVals, keptTypes = List.split3 keptTriples in
  let assignments = create_assignments constIds constVals in
  let tenv' =
     List.fold_left2 
      (fun tenv id t -> PMap.add id t tenv) 
      fundef.tenv constIds constTypes
  in   
  let fundef' = {
    fundef with body = assignments @ fundef.body;  
    input_ids = keptIds; 
    input_types = keptTypes; 
    tenv = tenv';  
  } in
  let fnT = DynType.FnT(keptTypes, fundef'.output_types) in  
  Some (fundef', fnT, keptVals)
  
let rec eval_block constEnv = function 
  | [] -> [], [], false
  | stmt::rest -> 
      let currStmts, types, changed = eval_stmt constEnv stmt in 
      let rest', restTypes, changedRest = eval_block constEnv rest in 
      currStmts @ rest', types@ restTypes, changed || changedRest 

and eval_stmt constEnv stmtNode = match stmtNode.stmt with  
  | Set(ids, rhs) -> 
      let rhs', assignments, types, changed = eval_exp constEnv rhs in
      let stmtNode' = { stmtNode with stmt = Set(ids, rhs')} in  
      assignments @ [stmtNode'], types, changed 
  | If _ -> failwith "if not yet implemented"

and eval_exp constEnv expNode =
  let nochange = expNode, [], [], false in  
  match expNode.exp with 
  | App(fn, args) -> 
    (match get_constant_value constEnv fn with 
      | Some (Lam fundef) 
        when List.length fundef.input_ids = List.length args ->
        (match specialize_function constEnv fundef args with 
          | None -> nochange 
          | Some (fundef', fnT, keptVals) -> 
             let fnId = ID.gen() in
             let expNode' = {expNode with exp = App(Var fnId, keptVals) } in 
             let assignNode = {stmt=Set([fnId], Value (Lam fundef'));
                stmt_src=None }
             in  
             let assignments = [assignNode] in   
             expNode', assignments, [fnId, fnT] , true 
        ) 
      | _ -> nochange 
     )
   | ArrayOp (Prim.Map, t, [fn], args) -> 
    
      (match get_constant_value constEnv fn with 
      | Some (Lam fundef) 
        when List.length fundef.input_ids = List.length args ->
        (match specialize_function constEnv fundef args with 
          | None -> nochange
          | Some (fundef', fnT, keptVals) -> 
            let fnId = ID.gen() in
            let assignments = 
              [{stmt=Set([fnId], Value (Lam fundef'));stmt_src=None}] in 
            let expNode' = 
              { expNode with exp = ArrayOp(Prim.Map, t, [Var fnId], keptVals) }
            in 
            expNode', assignments, [fnId, fnT], true
         )
      | _ -> nochange 
      )  
   | Value v -> 
      let v', changed = eval_value constEnv v in 
      {expNode with exp = Value v'}, [], [], changed 
   | _ -> nochange 
and eval_value constEnv vNode = match vNode.value with  
  | Lam fundef -> 
    let fundef', changed = eval_fundef constEnv fundef in 
    {vNode with value = Lam  fundef'}, changed 
  | v -> vNode, false
and eval_value_list constEnv = function
  | [] -> [], false
  | v::vs -> 
      let v', changed = eval_value constEnv v in 
      let vs', restChanged = eval_value_list constEnv vs in 
      v'::vs', changed || restChanged   
and eval_fundef constEnv fundef = 
  let body', types, changed = eval_block constEnv fundef.body in
  let tenv' = 
    List.fold_left (fun tenv (id, t) -> PMap.add id t tenv) fundef.tenv types in       
  {fundef with body = body'; tenv = tenv' }, changed
  
let function_cloning fundef = 
  let constEnv = TypedFindConstants.find_constants fundef.body in 
  eval_fundef constEnv fundef 
  *)