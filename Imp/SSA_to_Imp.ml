open DynType
open Base
open Imp

let rec translate_value idEnv vNode = match vNode.SSA.value with  
  | SSA.Var id -> Var (PMap.find id idEnv) 
  | SSA.Num n -> Const n  
  | _ -> failwith "[ssa->imp] value not implemented "


and translate_exp codegen idEnv expectedType expNode = 
  match expNode.SSA.exp with  
  | SSA.App({SSA.value=SSA.Prim (Prim.ScalarOp Prim.Select); 
             value_type =FnT(_::expectedType::_, _)},
            [cond; tVal; fVal]) -> 
      let cond' = translate_value idEnv cond in 
      let tVal' = translate_value idEnv tVal in 
      let fVal' = translate_value idEnv fVal in 
      Select (expectedType, cond', tVal', fVal')
  | SSA.App({SSA.value=SSA.Prim (Prim.ScalarOp op); 
             value_type =FnT(argT::_, [resultT])},
            vs) ->
      Op(op, resultT, argT, List.map (translate_value idEnv) vs)   
  | SSA.Cast (t, vNode) -> 
      Cast(t, vNode.SSA.value_type, translate_value idEnv vNode)   
  (*| SSA.Adverb (adverb, FnVal(fnid, _), args) ->
     let args' = List.map translate_value args in
     let fn = () in  
     translate_adverb codegen  adverb fn args' 
    *)   
  (*| SSA.ArrayOp (adverb, _, args) ->*) 
      (* when function is unknown we need to switch over all possible function 
         IDs...not yet implemented 
        
      failwith "defunctionalization not yet implemented"
  *)
  | SSA.Values [v] -> translate_value idEnv v   
  | SSA.Values [] -> failwith "[ssa->imp] unexpected empty value list"
   
  | SSA.Values _ ->  failwith "[ssa->imp] unexpected multiple return values "
  | _ -> failwith $ 
    Printf.sprintf 
      "[ssa->imp] typed core exp not yet implemented: %s"
      (SSA.exp_to_str expNode)
    
and translate_stmt idEnv codegen stmtNode =
  let get_imp_id ssaId t = 
    if PMap.mem ssaId idEnv then PMap.find ssaId idEnv 
    else codegen#fresh_id t 
  in  
  match stmtNode.SSA.stmt with
  | SSA.Set([id], expNode) ->
      (match expNode.SSA.exp_types with 
        | [t] -> 
          let id' = get_imp_id id t in  
          let exp' = translate_exp codegen idEnv t expNode in
          codegen#emit [set (Var id') exp'];
          PMap.add id id' idEnv
        | _ -> failwith "[ssa->imp] expected only single value on rhs of set"
       )
  | SSA.Set(ids, {SSA.exp=SSA.Values vs;SSA.exp_types=exp_types}) ->
      let rec flatten_assignment idEnv ids types vs =
        match ids, types, vs with 
          | id::restIds, t::restTypes, v::restValues ->
             let id' = get_imp_id id t in
             let rhs = translate_value idEnv v in 
             codegen#emit [set (Var id') rhs];
             let idEnv' = PMap.add id id' idEnv in 
             flatten_assignment idEnv' restIds restTypes restValues   
          | [], [], [] -> idEnv  
          | _ -> failwith "[ssa->imp] length mismatch in set stmt"
      in flatten_assignment idEnv ids exp_types vs
  | SSA.Set(_::_::_, _) -> 
      failwith "[ssa->imp] multiple return values not implemented"
  | _ -> failwith "[ssa->imp] stmt not implemented"      
      

and translate fn =
  let codegen  = new ImpCodegen.imp_codegen in
  let inputTypes = DynType.fn_input_types fn.SSA.fun_type in 
  let outputTypes = DynType.fn_output_types fn.SSA.fun_type in 
  let freshInputIds = 
    List.map codegen#fresh_input_id inputTypes   
  in 
  let freshOutputIds = List.map codegen#fresh_output_id  outputTypes in  
  let idEnv =  PMap.combine 
    (PMap.of_list (List.combine fn.SSA.input_ids freshInputIds)) 
    (PMap.of_list (List.combine fn.SSA.output_ids freshOutputIds)) in 
  let _ = List.fold_left 
    (fun idEnv stmt -> translate_stmt  idEnv codegen stmt) 
    idEnv
    fn.SSA.body in 
  codegen#finalize  
  