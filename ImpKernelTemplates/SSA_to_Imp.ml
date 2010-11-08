open DynType
open Base
open Imp

let rec translate_value idEnv vNode =
  let vExp =  match vNode.SSA.value with  
  | SSA.Var id -> Var (ID.Map.find id idEnv) 
  | SSA.Num n -> Const n  
  | _ -> failwith "[ssa->imp] value not implemented "
  in 
  { Imp.exp = vExp; Imp.exp_type = vNode.SSA.value_type }

and translate_exp codegen globalFunctions idEnv expectedType expNode = 
  let impExpNode = match expNode.SSA.exp with  
  | SSA.App({SSA.value=SSA.Prim (Prim.ScalarOp Prim.Select); 
             value_type =FnT(_::expectedType::_, _)},
            [cond; tVal; fVal]) -> 
      let cond' = translate_value idEnv cond in 
      let tVal' = translate_value idEnv tVal in 
      let fVal' = translate_value idEnv fVal in 
      select cond' tVal' fVal' 
  | SSA.App({SSA.value=SSA.Prim (Prim.ScalarOp op); 
             value_type =FnT(argT::_, _)},
            vs) ->
      let vs' = List.map (translate_value idEnv) vs in 
      if Prim.is_comparison op then 
        cmp_op op ~t:argT vs' 
      else
        typed_op op vs' 
  (* assume you only have one array over which you're mapping for now *)
  | SSA.App({SSA.value=SSA.Prim (Prim.ArrayOp Prim.Map);
            value_type = FnT([payloadT; arrT], [outputT])},
            [payload; arr]) ->
    (match payload.SSA.value with
      | SSA.Var fnId ->
        let fundef = FnTable.find fnId globalFunctions in
        let fundef' = translate_fundef globalFunctions fundef in
        let arr' = translate_value idEnv arr in
        let output = codegen#fresh_var outputT in
        let i = codegen#fresh_var Int32T in
        let n = codegen#fresh_var Int32T in
        let bodyBlock = [
          set i (int 0);
          set n (len arr');
          while_ (i <$ n) [SPLICE; set i (i +$ (int 1))]
        ] in
        codegen#splice_emit fundef' [|idx arr' i|] [|idx output i|] bodyBlock;
        output
      | _ -> failwith "[ssa->imp] Expected function identifier"
    )
  (* assume you only have one initial value, and one array to be reduced *)    
  | SSA.App({SSA.value=SSA.Prim (Prim.ArrayOp Prim.Reduce); 
             value_type =FnT([payloadT; initialT; arrT], [outputT])},
             [payload; initial; arr]) ->
    (match payload.SSA.value with 
	    | SSA.Var fnId -> 
	      let fundef = FnTable.find fnId globalFunctions in 
        let fundef' = translate_fundef globalFunctions fundef in 
	      let initial' = translate_value idEnv initial in 
	      let arr' = translate_value idEnv arr in
	      let acc = codegen#fresh_var initialT in
	      let i = codegen#fresh_var Int32T in
        let n = codegen#fresh_var Int32T in  
	      let bodyBlock = [
	        set i (int 0);
	        set n (len arr');  
	        set acc initial'; 
	        while_ (i <$ n) [SPLICE; set i (i +$ (int 1))] 
	      ] in 
        codegen#splice_emit fundef' [|acc; idx arr' i|] [|acc|] bodyBlock; 
	      acc
      | _ -> failwith "[ssa->imp] Expected function identifier"
    )
  | SSA.App({SSA.value=SSA.GlobalFn fnId}, _) -> 
      failwith $ 
        Printf.sprintf  
          "Encountered call to %s, global functions must be inlined"
          (SSA.FnId.to_str fnId)	 
  | SSA.Cast (t, vNode) -> cast t (translate_value idEnv vNode)  
  | SSA.Values [v] -> translate_value idEnv v
  | SSA.Values [] -> failwith "[ssa->imp] unexpected empty value list"
  | SSA.Values _ ->  failwith "[ssa->imp] unexpected multiple return values "
  | _ -> failwith $ 
    Printf.sprintf 
      "[ssa->imp] typed core exp not yet implemented: %s"
      (SSA.exp_to_str expNode)
  in 
  if impExpNode.exp_type <> List.hd expNode.SSA.exp_types then 
    failwith $ 
    Printf.sprintf "[ssa->imp] mismatch between %s and %s while translating %s"
    (DynType.to_str impExpNode.exp_type)
    (DynType.to_str $ List.hd expNode.SSA.exp_types)
    (SSA.exp_to_str expNode) 
  else  
  impExpNode
    
and translate_stmt globalFunctions idEnv codegen stmtNode =
  let get_imp_id ssaId t = 
    if ID.Map.mem ssaId idEnv then ID.Map.find ssaId idEnv 
    else codegen#fresh_local_id t 
  in  
  match stmtNode.SSA.stmt with
  | SSA.Set([id], expNode) ->
      (match expNode.SSA.exp_types with 
        | [t] -> 
          let id' = get_imp_id id t in  
          let exp' =
             translate_exp codegen globalFunctions idEnv t expNode 
          in
          let varNode = {Imp.exp = Var id'; exp_type = t } in 
          codegen#emit [set varNode exp'];
          ID.Map.add id id' idEnv
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
             let idEnv' = ID.Map.add id id' idEnv in 
             flatten_assignment idEnv' restIds restTypes restValues   
          | [], [], [] -> idEnv  
          | _ -> failwith "[ssa->imp] length mismatch in set stmt"
      in flatten_assignment idEnv ids exp_types vs
  | SSA.Set(_::_::_, _) -> 
      failwith "[ssa->imp] multiple return values not implemented"
  | _ -> failwith "[ssa->imp] stmt not implemented"      
      

and translate_fundef globalFunctions fn =
  let codegen  = new ImpCodegen.imp_codegen in
  let inputTypes = DynType.fn_input_types fn.SSA.fn_type in 
  let outputTypes = DynType.fn_output_types fn.SSA.fn_type in 
  let freshInputIds = 
    List.map codegen#fresh_input_id inputTypes   
  in 
  let freshOutputIds = List.map codegen#fresh_output_id  outputTypes in  
  let idEnv = ID.Map.combine 
    (ID.Map.of_list (List.combine fn.SSA.input_ids freshInputIds)) 
    (ID.Map.of_list (List.combine fn.SSA.output_ids freshOutputIds)) in 
  let _ = List.fold_left
    (fun idEnv stmt -> translate_stmt globalFunctions idEnv codegen stmt) 
    idEnv
    fn.SSA.body 
  in 
  codegen#finalize  
  
