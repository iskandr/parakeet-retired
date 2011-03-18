(* pp: -parser o pa_macro.cmo *)

open DynType
open Base
open Imp

module type PARAMS = sig
  val fnTable : FnTable.t
  val fnState : ImpCodegen.fn_state 
  val sizeEnv : SymbolicShape.shape ID.Map.t  
  val idEnv : ID.t ID.Map.t
  val translate_fundef : SSA.fundef -> Imp.fn 
end

module MkTranslator(P : PARAMS) = struct 
    let get_id ssaId = ID.Map.find ssaId P.idEnv
    
    (* TODO: make this actually work with non-trivial expressions *)
		let get_shape (expNode : Imp.exp_node) = match expNode.exp with  
		  | Imp.Var id ->
		      try ID.Map.find id P.sizeEnv with 
		        | _ -> failwith $ 
		          Printf.sprintf  
		            "Can't find shape for Imp variable %s : %s"
		            (ID.to_str id)
		            (DynType.to_str expNode.exp_type) 
		  | _ -> SymbolicShape.scalar
		
		let translate_value valNode = 
		  let vExp = match valNode.SSA.value with
		    | SSA.Var ssaId -> Imp.Var (get_id ssaId)
		    | SSA.Num n -> Imp.Const n
		    | _ -> failwith "[ssa->imp] value not implemented "
		  in
		  { Imp.exp = vExp; Imp.exp_type = valNode.SSA.value_type }
		
    let translate_values vs = List.map translate_value vs 
    
		let translate_exp 
          (codeBuffer : ImpCodegen.code_buffer) 
          (expectedType : DynType.t) 
          (expNode : SSA.exp_node) = 
		  let impExpNode = match expNode.SSA.exp with
		  | SSA.Values [v] -> translate_value v
		  | SSA.Values [] -> failwith "[ssa->imp] unexpected empty value list"
		  | SSA.Values _ ->  failwith "[ssa->imp] unexpected multiple return values"
		    
		  | SSA.PrimApp (Prim.ScalarOp Prim.Select, [cond; tVal; fVal]) ->  
		      let cond' = translate_value cond in 
		      let tVal' = translate_value tVal in 
		      let fVal' = translate_value fVal in 
		      select cond' tVal' fVal' 
		  
		  | SSA.PrimApp (Prim.ScalarOp op, vs) -> 
		      let vs' = translate_values vs in 
		      let argT = (List.hd vs').exp_type in  
		      if Prim.is_comparison op then cmp_op op ~t:argT vs' 
		      else typed_op op vs'
		      
		  | SSA.PrimApp (Prim.ArrayOp Prim.DimSize, [array; idx]) -> 
          (match idx.SSA.value with 
            | SSA.Num n ->  
		          let array' = translate_value array in 
		          let i = PQNum.to_int n in 
		          Imp.dim i array'
            | _ -> 
              failwith $
                "[ssa->imp] DimSize with non-constant index not yet implemented"
          ) 
		  | SSA.PrimApp (Prim.ArrayOp Prim.Index, [inArray; idx]) -> 
          if DynType.is_scalar $ idx.SSA.value_type then 
            Imp.idx (translate_value inArray) (translate_value idx)
          else 
            failwith "[ssa->imp] Vector indexing not implement" 
               
		  | SSA.PrimApp (Prim.ArrayOp Prim.Find, [inArray; elToFind]) -> 
		    let arrT = inArray.SSA.value_type in 
		    let valT = elToFind.SSA.value_type in 
		    let inArray' = translate_value inArray in
		    let elToFind' = translate_value elToFind in
		    let i = P.fnState#fresh_var Int32T in
		    let index = P.fnState#fresh_var Int32T in
		    let n = P.fnState#fresh_var Int32T in
		    codeBuffer#emit [
		      set i (int 0);
		      set n (len inArray');
		      set index n;
		      while_ ((i <$ n) &&$ (index =$ n)) [
		        ifTrue ((idx inArray' i) =$ elToFind') [set index i]
		      ]
		    ];
		    index
		  | SSA.PrimApp (Prim.ArrayOp Prim.Where, [_]) -> 
		    failwith ("WHERE operator can't be translated to Imp " ^ 
		              "due to unpredictable size semantics") 
		
		  
		  | SSA.Cast (t, vNode) -> cast t (translate_value vNode) 
		  (* assume you only have one array over which you're mapping for now *)
		  | SSA.Map(payload, arrays) ->
		      let eltOutTypes = payload.SSA.closure_output_types in
		      (* TODO: make this work for multiple outputs *)  
		      let eltOutType = List.hd eltOutTypes in
		      let vecOutType = DynType.VecT eltOutType in 
		      let fundef_ssa = FnTable.find payload.SSA.closure_fn P.fnTable in
		      let fundef_imp = P.translate_fundef fundef_ssa in
		      let arrays_imp = translate_values arrays in
		      let maxArray = SymbolicShape.largest_val (Array.of_list arrays_imp) in 
		      let maxArrayShape = get_shape maxArray in
		      let maxDim = List.hd maxArrayShape in
          let impOutputId = fundef_imp.Imp.output_ids.(0) in 
		      let nestedOutShape =
		          Hashtbl.find_default fundef_imp.sizes impOutputId []
		      in  
		      let outputShape = maxDim :: nestedOutShape in   
		      let output = 
		        P.fnState#fresh_var 
		          ~dims:outputShape    
		          ~storage:Imp.Private
		          vecOutType 
		      in  
		      let i = P.fnState#fresh_var Int32T in
		      let n = P.fnState#fresh_var Int32T in
		      (* since shapes are imp expressions, we can just take the outermost
		         dim of the symbolic shape and put it as the loop boundary *) 
		      let loopBoundary = List.hd maxArrayShape in 
		      let bodyBlock = [
		        set i (int 0);
		        set n loopBoundary;
		        while_ (i <$ n) [SPLICE; set i (i +$ (int 1))]
		      ] in
		      let lhs = [|idx output i|] in
		      let closureArgs = 
		        translate_values payload.SSA.closure_args 
		      in  
		      let rhs = Array.of_list $ 
		        closureArgs @ (List.map (fun arr -> idx arr i) arrays_imp) 
		      in 
		      codeBuffer#splice_emit fundef_imp rhs lhs bodyBlock;
		      output
		  
		  (* assume you only have one initial value, and only one scalar output *)    
		  | SSA.Reduce(initClosure, closure, initArgs, arrays) ->
		      (* TODO: make use of initClosure *)  
		      let accTypes = closure.SSA.closure_output_types in    
		      let arrayTypes = List.map (fun v -> v.SSA.value_type) arrays in 
		      (* TODO: make this work for multiple outputs *)  
		      let outputType = List.hd accTypes in
		      let initial = List.hd initArgs in 
		      let fundef = FnTable.find closure.SSA.closure_fn P.fnTable in 
		      let impPayload = P.translate_fundef fundef in 
		      let impInit = translate_value initial in
		      assert (arrays <> []);  (* assume at least one array *) 
		      let impArrays = translate_values arrays in
		      (* for now assume acc is a scalar *) 
		      let acc = P.fnState#fresh_var outputType in
		      let i = P.fnState#fresh_var Int32T in
		      let n = P.fnState#fresh_var Int32T in  
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
		      codeBuffer#splice_emit impPayload payloadArgs payloadOutputs bodyBlock;
		      acc
		    
		  | _ -> failwith $ 
		    Printf.sprintf 
		      "[ssa->imp] typed core exp not yet implemented: %s"
		      (SSA.exp_to_str expNode)
		  in 
		  impExpNode
		
    let translate_set codeBuffer ssaId rhsExp =  
      let id' = get_id ssaId in
      let t = List.hd rhsExp.SSA.exp_types in 
      let exp' = translate_exp codeBuffer t rhsExp in
      let varNode = {Imp.exp = Var id'; exp_type = t } in 
      codeBuffer#emit [set varNode exp']
    
    let translate_set_val codeBuffer ssaId rhsVal = 
      let impId = get_id ssaId in
      let rhs = translate_value rhsVal in
      let t = rhsVal.SSA.value_type in 
      codeBuffer#emit [set (var ~t impId) rhs]
    
    let translate_phi_node codeBuffer pred phiNode  = 
      let id = phiNode.SSA.phi_id in 
      match pred.exp with 
      | Imp.Const (PQNum.Bool true) ->
        translate_set_val codeBuffer id phiNode.SSA.phi_left
      | Imp.Const (PQNum.Bool false) -> 
        IFDEF DEBUG THEN 
          Printf.printf "Generating right side of phi node: %s <- %s\n"
            (ID.to_str id)
            (SSA.value_node_to_str phiNode.SSA.phi_right);
        ENDIF; 
        translate_set_val codeBuffer id phiNode.SSA.phi_right
      | _ -> 
        let impId = get_id phiNode.SSA.phi_id in 
        let impLeft = translate_value phiNode.SSA.phi_left in 
        let impRight = translate_value phiNode.SSA.phi_right in
        codeBuffer#emit [ 
          Imp.If(pred, [Imp.Set(impId, impLeft)], [Imp.Set(impId, impRight)])
        ] 
    let translate_phi_nodes codeBuffer pred phiNodes = 
      List.iter (translate_phi_node codeBuffer pred) phiNodes 
    
		let rec translate_stmt 
              (codeBuffer : ImpCodegen.code_buffer) 
              (stmtNode : SSA.stmt_node) = 
      match stmtNode.SSA.stmt with
		  | SSA.Set([id], expNode) ->
		      if List.length expNode.SSA.exp_types <> 1 then 
            failwith "[ssa->imp] expected only single value on rhs of set"
          else translate_set codeBuffer id expNode 
		  | SSA.Set(ids, ({SSA.exp=SSA.Values vs} as expNode)) ->
          IFDEF DEBUG THEN
            let valTypes = List.map (fun vNode -> vNode.SSA.value_type) vs in 
            assert (List.for_all2 (=) valTypes expNode.SSA.exp_types); 
          ENDIF;    
          List.iter2 (translate_set_val codeBuffer) ids vs 
		  | SSA.Set(_::_::_, _) -> 
		      failwith "[ssa->imp] multiple return values not implemented"
      | SSA.If(testVal, tBlock, fBlock, phiNodes) -> 
          let testVal' = translate_value testVal in 
          let tBuffer = P.fnState#fresh_code_buffer in  
          translate_block tBuffer tBlock; 
          let fBuffer = P.fnState#fresh_code_buffer in   
          translate_block fBuffer fBlock; 
          codeBuffer#emit [
            (Imp.If(testVal', tBuffer#to_block, fBuffer#to_block)) 
          ]; 
          translate_phi_nodes codeBuffer testVal' phiNodes
		  | SSA.WhileLoop(testBlock, testVal, body, header, exit) ->
          (* first translate only the lhs assignments of the phi nodes, 
             since we are entering the loop 
          *)
          translate_phi_nodes codeBuffer (Imp.bool true) header;
          let testExp = translate_value testVal in 
          translate_block codeBuffer testBlock; 
          let bodyBuffer = P.fnState#fresh_code_buffer in 
          translate_block bodyBuffer body; 
          translate_phi_nodes bodyBuffer (Imp.bool false); header; 
          translate_block bodyBuffer testBlock;
          let block = bodyBuffer#to_block in 
          codeBuffer#emit [Imp.While(testExp, block)] 
          
		     
    and translate_block codeBuffer block =
      Block.iter_forward (translate_stmt codeBuffer) block 
end 


let rec translate_fundef fnTable fn =
  IFDEF DEBUG THEN 
    Printf.printf "[SSA_To_Imp] translating function: %s\n"
      (SSA.fundef_to_str fn);
  ENDIF; 
    
  let fnState = new ImpCodegen.fn_state in
  let inputTypes = fn.SSA.fn_input_types in 
  let outputTypes = fn.SSA.fn_output_types in
  (* first generate imp ids for inputs, to make sure their order is preserved *)
  let add_input env id t = 
    let impId = fnState#fresh_input_id t in 
    IFDEF DEBUG THEN 
      Printf.printf "[SSA_To_Imp] Renaming input %s => %s\n"
        (ID.to_str id)
        (ID.to_str impId); 
    ENDIF; 
    ID.Map.add id impId env  
  in 
  let inputIdEnv = 
    List.fold_left2 add_input ID.Map.empty fn.SSA.input_ids inputTypes   
  in 
  (* next add all the live locals, along with their size expressions *)
  let liveIds : ID.t MutableSet.t = FindLiveIds.find_live_ids fn in 
  let sizeEnv : SymbolicShape.shape ID.Map.t = 
    ShapeInference.infer_shape_env fnTable fn  
  in
  let add_local id env =
    (* ignore inputs since they were already handled *) 
    if List.mem id fn.SSA.input_ids then env
    else 
    let t = ID.Map.find id fn.SSA.tenv in
    (* WARNING: Hack! Assume that a shape that's not in the env is a scalar *) 
    let dims : Imp.exp_node list = 
      ID.Map.find_default id sizeEnv SymbolicShape.scalar in
    (* rename all vars to refer to new Imp IDs, instead of old SSA IDs *)
    let dims' : Imp.exp_node list =
       List.map (ImpReplace.apply_id_map env) dims 
    in   
    let impId = 
      (if List.mem id fn.SSA.output_ids then 
        fnState#fresh_output_id ~dims:dims' t 
      else
        fnState#fresh_local_id ~dims:dims' t
      )
    in  
    
    IFDEF DEBUG THEN 
        Printf.printf "[ssa2imp] Renamed %s to %s\n"
          (ID.to_str id)
          (ID.to_str impId); 
    ENDIF;
    ID.Map.add id impId env    
  in  
  let idEnv = MutableSet.fold add_local liveIds inputIdEnv in
  let rename_shape_env id shape env =
    let id' = ID.Map.find id idEnv in
    let shape' = List.map (ImpReplace.apply_id_map idEnv) shape in   
    ID.Map.add id' shape' env  
  in
  let impSizeEnv = ID.Map.fold rename_shape_env sizeEnv ID.Map.empty in 
  (*IFDEF DEBUG THEN 
    Printf.printf "[ssa2imp] Size env\n";
    let print_size id sz =
      Printf.printf "[ssa2imp] %s : %s\n"
        (ID.to_str id)
        (SymbolicShape.to_str sz)
    in 
    ID.Map.iter print_size impSizeEnv
  ENDIF;
  *)  
  let module Translator = MkTranslator(struct
    let fnTable = fnTable 
    let idEnv = idEnv
    let sizeEnv = impSizeEnv
    let fnState = fnState 
    let translate_fundef = (translate_fundef fnTable)
  end) 
  in 
  Translator.translate_block fnState#main_code_buffer fn.SSA.body; 
  fnState#finalize 