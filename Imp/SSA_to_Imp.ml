open DynType
open Base
open Imp

let rec translate_value idEnv vNode =
  let vExp =  match vNode.SSA.value with  
  | SSA.Var id -> Var (PMap.find id idEnv) 
  | SSA.Num n -> Const n  
  | _ -> failwith "[ssa->imp] value not implemented "
  in 
  { Imp.exp = vExp; Imp.exp_type = vNode.SSA.value_type }

and translate_exp codegen idEnv expectedType expNode = 
  let impExp = match expNode.SSA.exp with  
  | SSA.App({SSA.value=SSA.Prim (Prim.ScalarOp Prim.Select); 
             value_type =FnT(_::expectedType::_, _)},
            [cond; tVal; fVal]) -> 
      let cond' = translate_value idEnv cond in 
      let tVal' = translate_value idEnv tVal in 
      let fVal' = translate_value idEnv fVal in 
      Select (expectedType, cond', tVal', fVal')
  | SSA.App({SSA.value=SSA.Prim (Prim.ScalarOp op); 
             value_type =FnT(argT::_, _)},
            vs) ->
      Op(op, argT, List.map (translate_value idEnv) vs)   
  | SSA.Cast (t, vNode) -> 
      Cast(t, translate_value idEnv vNode)   
  | SSA.Values [v] -> 
      let vNode = translate_value idEnv v in 
      vNode.Imp.exp    
  | SSA.Values [] -> failwith "[ssa->imp] unexpected empty value list"
   
  | SSA.Values _ ->  failwith "[ssa->imp] unexpected multiple return values "
  | _ -> failwith $ 
    Printf.sprintf 
      "[ssa->imp] typed core exp not yet implemented: %s"
      (SSA.exp_to_str expNode)
  in 
  { Imp.exp = impExp; Imp.exp_type = List.hd expNode.SSA.exp_types } 
    
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
          let varNode = {Imp.exp = Var id'; exp_type = t } in 
          codegen#emit [set varNode exp'];
          PMap.add id id' idEnv
        | _ -> failwith "[ssa->imp] expected only single value on rhs of set"
       )
  | SSA.Set(ids, {SSA.exp=SSA.Values vs;SSA.exp_types=exp_types}) ->
      let rec flatten_assignment idEnv ids types vs =
        match ids, types, vs with 
          | id::restIds, t::restTypes, v::restValues ->
             let id' = get_imp_id id t in
             let rhs = translate_value idEnv v in
             let varNode = { Imp.exp = Var id'; Imp.exp_type = t} in  
             codegen#emit [set varNode rhs];
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
  