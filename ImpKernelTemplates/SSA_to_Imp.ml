(* pp: -parser o pa_macro.cmo *)

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
  | SSA.App({SSA.value=SSA.Prim (Prim.ScalarOp Prim.Select)} as fnNode,
            [cond; tVal; fVal]) ->
      let cond' = translate_value idEnv cond in 
      let tVal' = translate_value idEnv tVal in 
      let fVal' = translate_value idEnv fVal in 
      select cond' tVal' fVal' 
  | SSA.App({SSA.value=SSA.Prim (Prim.ScalarOp op) } as fnNode,  vs) ->
      let vs' = List.map (translate_value idEnv) vs in 
      let argT = (List.hd vs').exp_type in  
      if Prim.is_comparison op then cmp_op op ~t:argT vs' 
      else typed_op op vs' 
  (* assume you only have one array over which you're mapping for now *)
  | SSA.App({SSA.value=SSA.Prim (Prim.ArrayOp Prim.Map)} as fnNode,
            payload :: arrays) ->
    IFDEF DEBUG THEN 
      assert (DynType.is_function fnNode.SSA.value_type); 
      assert (DynType.is_function payload.SSA.value_type); 
      assert (DynType.fn_output_arity fnNode.SSA.value_type > 0); 
    ENDIF; 
    let outputTypes = DynType.fn_output_types fnNode.SSA.value_type in 
    (* TODO: make this work for multiple outputs *)  
    let outputType = List.hd outputTypes in
    (match payload.SSA.value with
      | SSA.GlobalFn fnId ->
        let fundef_ssa = FnTable.find fnId globalFunctions in
        let fundef_imp = translate_fundef globalFunctions fundef_ssa in
        let arrays_imp = List.map (translate_value idEnv) arrays in
        let maxInput = largest_val (Array.of_list arrays_imp) in 
        let output =  
          codegen#fresh_array_output outputType (all_dims maxInput) 
        in 
        let i = codegen#fresh_var Int32T in
        let n = codegen#fresh_var Int32T in
        let bodyBlock = [
          set i (int 0);
          set n (len maxInput);
          while_ (i <$ n) [SPLICE; set i (i +$ (int 1))]
        ] in
        let lhs = [|idx output i|] in 
        let rhs = Array.of_list (List.map (fun arr -> idx arr i) arrays_imp) in 
        codegen#splice_emit fundef_imp rhs lhs bodyBlock;
        output
      | _ -> failwith "[ssa->imp] Expected function identifier"
    )
  | SSA.App({SSA.value=SSA.Prim (Prim.ArrayOp Prim.Map)}, _) -> 
      failwith "Map not implemented"

  (* assume you only have one initial value, and only one scalar output *)    
  | SSA.App({SSA.value=
               SSA.Prim (Prim.ArrayOp Prim.Reduce)} as fnNode,  
             payload::initial::arrays) ->
    let initialT = initial.SSA.value_type in 
    let arrayTypes = List.map (fun v -> v.SSA.value_type) arrays in 
    let outputTypes = DynType.fn_output_types fnNode.SSA.value_type in
    (* TODO: make this work for multiple outputs *)  
    let outputType = List.hd outputTypes in   
    (match payload.SSA.value with 
	    | SSA.GlobalFn fnId -> 
	      let fundef = FnTable.find fnId globalFunctions in 
        let impPayload = translate_fundef globalFunctions fundef in 
	      let impInit = translate_value idEnv initial in
        assert (arrays <> []);  (* assume at least one array *) 
	      let impArrays = List.map (translate_value idEnv) arrays in
        (* for now assume acc is a scalar *) 
	      let acc = codegen#fresh_var initialT in
	      let i = codegen#fresh_var Int32T in
        let n = codegen#fresh_var Int32T in  
        (* alex: fixing a bug wherein the "arrays" are actually scalars *)
        IFDEF DEBUG THEN 
          assert (List.length impArrays = List.length arrayTypes); 
        ENDIF; 
        let arrayElts = 
            List.map2 
              (fun arr t -> if DynType.is_scalar t then arr else idx arr i) 
              impArrays
              arrayTypes   
        in
        let payloadArgs = Array.of_list (acc :: arrayElts) in
        let payloadOutputs = [|acc|] in
        let bodyBlock = 
          if List.exists DynType.is_vec arrayTypes then  
	        [
	          set i (int 0);
            (* assume arrays are of the same length *) 
	          set n (len $ List.hd impArrays);  
	          set acc impInit; 
	          while_ (i <$ n) [SPLICE; set i (i +$ (int 1))] 
	        ]
          (* if all arguments are scalars, just call the function directly *)
          else [SPLICE] 
        in 
        codegen#splice_emit impPayload payloadArgs payloadOutputs bodyBlock;
        acc
      | _ -> failwith "[ssa->imp] Expected function identifier"
    )
  | SSA.App({SSA.value=
               SSA.Prim (Prim.ArrayOp Prim.Find)}, 
            [inArray; elToFind]) ->
    let arrT = inArray.SSA.value_type in 
    let valT = elToFind.SSA.value_type in 
    let inArray' = translate_value idEnv inArray in
    let elToFind' = translate_value idEnv elToFind in
    let i = codegen#fresh_var Int32T in
    let index = codegen#fresh_var Int32T in
    let n = codegen#fresh_var Int32T in
    codegen#emit [
      set i (int 0);
      set n (len inArray');
      set index n;
      while_ ((i <$ n) &&$ (index =$ n)) [
        ifTrue ((idx inArray' i) =$ elToFind') [set index i]
      ]
    ];
    index
  | SSA.App({SSA.value=SSA.GlobalFn fnId}, _) -> 
      failwith $ 
        Printf.sprintf  
          "Encountered call to %s, global functions must be inlined"
          (FnId.to_str fnId)	 
  | SSA.Cast (t, vNode) -> cast t (translate_value idEnv vNode)  
  | SSA.Values [v] -> translate_value idEnv v
  | SSA.Values [] -> failwith "[ssa->imp] unexpected empty value list"
  | SSA.Values _ ->  failwith "[ssa->imp] unexpected multiple return values "
  | _ -> failwith $ 
    Printf.sprintf 
      "[ssa->imp] typed core exp not yet implemented: %s"
      (SSA.exp_to_str expNode)
  in 
  (*if impExpNode.exp_type <> List.hd expNode.SSA.exp_types then 
    failwith $ 
    Printf.sprintf "[ssa->imp] mismatch between %s and %s while translating %s"
    (DynType.to_str impExpNode.exp_type)
    (DynType.to_str $ List.hd expNode.SSA.exp_types)
    (SSA.exp_to_str expNode) 
  else
    *)  
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
  IFDEF DEBUG THEN 
     Printf.printf 
       "Translating function into Imp of type %s->%s\n"
       (DynType.type_list_to_str inputTypes)
       (DynType.type_list_to_str outputTypes)
     ;
  ENDIF;
  (* jesus, unspecified evaluation order and side effects really don't mix--
     Beware of List.map!  
  *)
  let inputIdEnv = 
    List.fold_left2 
      (fun accEnv ssaId t -> 
        let impId = codegen#fresh_input_id t in
        ID.Map.add ssaId impId accEnv
      )
      ID.Map.empty 
      fn.SSA.input_ids  
      inputTypes
  in 
  let idEnv = 
    List.fold_left2
      (fun accEnv ssaId t -> 
          let impId = codegen#fresh_output_id t in 
          ID.Map.add ssaId impId accEnv 
       )
       inputIdEnv 
       fn.SSA.output_ids 
       outputTypes 
  in  
  let _ = List.fold_left
    (fun idEnv stmt -> translate_stmt globalFunctions idEnv codegen stmt) 
    idEnv
    fn.SSA.body 
  in 
  codegen#finalize
  
  
