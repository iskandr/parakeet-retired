(* pp: -parser o pa_macro.cmo *)
open Base
open SSA 

module type TYPE_ANALYSIS_PARAMS = sig 
  val interpState : InterpState.t
  val closures : (ID.t, value) Hashtbl.t
  val closureArgs : (ID.t, ID.t list) Hashtbl.t
  val closureArity : (ID.t, int) Hashtbl.t
  val infer_output_types : value -> Signature.t -> DynType.t list   
  val signature : Signature.t 
end
  
let get_type tenv id = 
  if ID.Map.mem id tenv 
  then ID.Map.find id tenv 
  else DynType.BottomT

(* TODO: make this complete for all SSA statements *) 
let rec is_scalar_stmt = function
  | SSA.Set(_, {exp=SSA.App({value=SSA.Prim (Prim.ScalarOp _)}, _)})
  | SSA.Set(_, {exp=Values _}) -> true 
  | SSA.If(_, tCode, fCode, _) -> 
      all_scalar_stmts tCode && all_scalar_stmts fCode
  | _ -> false   
and all_scalar_stmts = List.for_all is_scalar_stmt    

module MkAnalysis (P : TYPE_ANALYSIS_PARAMS) = struct
  type env = DynType.t ID.Map.t 
  type exp_info = DynType.t list
  type value_info = DynType.t 
  
  let init fundef =
    let inTypes = Signature.input_types P.signature in  
    let tenv = ID.Map.extend ID.Map.empty fundef.input_ids inTypes in   
    if Signature.has_output_types P.signature then 
      let outTypes = Signature.output_types signature in 
      ID.Map.extend tenv fundef.output_ids outTypes 
    else tenv
   
  let value tenv = function 
    | Var id -> get_type context id
    | Num _ -> PQNum.type_of_num n
    | Str _ -> DynType.StrT
    | Sym _ -> DynType.SymT
    | Unit -> DynType.UnitT
    | _ -> DynType.AnyT   
  
  let exp tenv expNode info = match expNode, info with 
    | App(fn, args), AppInfo(fnT, argTypes)->
      (match fn.value  with 
      | Var id ->
          (* if the identifier would evaluate to a function value...*) 
          if Hashtbl.mem P.closures id then
            let fnVal = Hashtbl.find P.closures id in 
            let closureArgIds = Hashtbl.find P.closure_args in 
            let closureArgTypes = List.map (get_type tenv) closureArgIds in 
            let signature = 
              Signature.from_input_types (closureArgTypes@argTypes) 
            in 
            P.infer_output_types fnVal signature  
             
          else if DynType.is_vec fnType then   
            (* if ID doesn't evaluate to a function, assume it evaluates to 
               an array 
            *) 
            [TypeInfer.infer_simple_array_op Prim.Index (fnType::argTypes)]   
          else assert false 
      | Prim.ArrayOp Prim.Map -> failwith "map not implemented"
      | Prim.ArrayOp Prim.Reduce -> failwith "reduce not implemented"
      | Prim.ArrayOp Prim.Scan -> failwith "scan not implemented" 
      | fnVal -> 
          let signature = Signature.from_input_types argTypes in
          let typedFn = T.specialize fnVal signature in
          typedFn.output_types     
     )
    | _, ArrayInfo eltTypes ->
      let commonT = DynType.fold_type_list eltTypes in 
      assert (commonT <> DynType.AnyT); 
      DynType.VecT commonT
    | _, ValuesInfo types -> types  
    | _ -> failwith "not implemented"
  
  let stmt context stmtNode stmtInfo = match stmtNode, stmtInfo with 
    | Set(ids, _), SetInfo rhsTypes ->               
        IFDEF DEBUG THEN
          if List.length ids <> List.length rhsTypes then 
          failwith $ sprintf 
            "malformed SET statement: %d ids for %d rhs values \n"
            (List.length ids)
            (List.length rhsTypes)
        ENDIF; 
      let rec process_types (tenv, changed) id rhsT =  
        IFDEF DEBUG THEN 
          if rhsT = DynType.AnyT then failwith "error during type inference"
        ENDIF; 
        let oldT = get_type tenv id in 
        let newT = DynType.common_type oldT rhsT in 
        let changedT = oldT <> newT in
        let tenv' = 
          if changedT then ID.Map.add id newT tenv else tenv 
        in 
        tenv', (changed || changedT)
      in 
      let tenv', changed = 
        List.fold_left2 process_types (context.type_env, false) ids rhsTypes
      in  
      if changed then Some { context with type_env = tenv' } else None 
    | _ -> failwith "not implemented"
end

type specializer = SSA.value -> Signature.t -> SSA.fundef
   
let type_analysis interpState infer_output_types closureEnv fundef signature = 
  let module Params : TypeAnalysis.TYPE_ANALYSIS_PARAMS = struct 
    let interpState = interpState
    let closures = closureEnv.CollectPartialApps.closures
    let closureArgs = closureEnv.CollectPartialApps.closure_args 
    let closureArity = closureEnv.CollectPartialApps.closure_arity 
    let infer_output_types = infer_output_types
    let signature = signature  
  end    
  in
  let module TypeEval = MkEvaluator(MkAnalysis(Params)) in 
  TypeEval.eval_fundef fundef 
    

let annotate_value context valNode = 
  
  let t = match valNode.value with 
  | Var id -> ID.Map.find id context.type_env  
  | Prim _ ->
    failwith "[Specialize] cannot annotate primitive without its arguments"
  | GlobalFn _ 
  | Lam _ -> 
      failwith "[Specialize] cannot annotate a function without its arguments"
  | other -> type_of_raw_value other  
 in 
    (* if the new type is different from the old, *)
    (* then indicate that a change has happened *)    
    let changed = valNode.value_type <> t in 
    let valNode' = {valNode with value_type = t} in  
    valNode', t, changed  

let value_to_sig_elt context v : Signature.sig_elt = 
  match v with  
  | Prim p -> Signature.Closure(Prim p, [])
  | GlobalFn fnId -> Signature.Closure (GlobalFn fnId, [])
  | Lam _ -> failwith "lambda should have been lifted to top-level"
  | Var id -> 
      if ID.Map.mem id context.untyped_closures then
        let fnVal, closureArgs = ID.Map.find id context.untyped_closures in
        Signature.Closure(fnVal, closureArgs)
      else 
        Signature.Type (ID.Map.find id context.type_env)
  | other -> Signature.Type (type_of_raw_value other) 

let rec value_nodes_to_sig_elts context = function 
  | v::rest -> 
    (value_to_sig_elt context v.value)::(value_nodes_to_sig_elts context rest)
  | [] -> [] 
 
(**************************************************************************)
(*       SPECIALIZE A SCALAR OPERATOR FOR ALL SCALAR ARGUMENT TYPES       *)
(**************************************************************************) 
(* called by specialize_function_value, doesn't check if specialization   *) 
(* already exists.  *)
                
let specialize_scalar_prim_for_scalar_args 
    program
    op 
    ?forceOutputType 
    inputTypes
  : SSA.fundef  = 
  let expectedTypes = TypeInfer.required_scalar_op_types op inputTypes in 
  let inferredOutputT = TypeInfer.infer_scalar_op op inputTypes in
  let typedPrim = { 
    value = Prim (Prim.ScalarOp op); 
    value_type = DynType.FnT(expectedTypes, [inferredOutputT]); 
    value_src = None; 
  }
  in  
  
  (* the list of output types is used for constructing specialization *)
  (* signatures, supplying arguments types to mk_lambda and creating the *)
  (* FnT type associated with the returned value. *) 
  let outputTypes = [Option.default inferredOutputT forceOutputType] in   
  mk_lambda inputTypes outputTypes 
    (fun codegen inputs outputs ->
        let convertedInputs = List.map3 
            (fun var oldType newType -> 
                codegen#cvt ~to_type:newType ~from_type:oldType var)
            inputs inputTypes expectedTypes 
        in  
        let appNode = 
          mk_app ~types:[inferredOutputT] typedPrim convertedInputs  
        in 
        codegen#emit [mk_set (get_ids outputs) appNode]
    ) 
     

(*
   THIS IS THE HEART OF THE TYPE INFERENCE / SPECIALIZATION ALGORITHM:
   ANNOTATION AND SPECIALIZATION ARE BOUND TOGETHER THROUGH POLYVARIANCE. 
   ----------------------------------------------------------------------
   Polyvariance means that at every function call site we specialize 
   that function for that particular argument signature. In the case 
   of higher order functions the function values are included in the 
   signature. 
*) 
   

let rec annotate_app context expSrc fn args =
  IFDEF DEBUG THEN Printf.printf "ANNOTATE APP!%!\n"; ENDIF;
   match fn.value with
  (* for now assume that all higher order primitives take a single *)
  (* function argument which is passed first *)  
  | Prim (Prim.ArrayOp p) when Prim.is_higher_order p -> 
      IFDEF DEBUG THEN Printf.printf "HIGHER ORDER!%!\n"; ENDIF;
      IFDEF DEBUG THEN 
        assert (List.length args > 1);
      ENDIF;
      let dataArgs = List.tl args in 
      let dataArgs', types, argsChanged = annotate_values context dataArgs in
      IFDEF DEBUG THEN Printf.printf "HIGHER ORDER 2!%!\n"; ENDIF;
      (* for now just pass through the literal value but should eventually*)
      (* have a context available with an environment of literal value *)
      (* arguments and check whether the fnArg is in that environment. *)
      let fnVal = (List.hd args).value in 
      IFDEF DEBUG THEN
        let ok = match fnVal with
          | SSA.GlobalFn _ | SSA.Prim _ -> true
          | SSA.Var id -> ID.Map.mem id context.untyped_closures  
          | _ -> false
        in 
        assert ok  
      ENDIF;  
      let fnSigElt = match fnVal with 
        | GlobalFn _ 
        | Prim _ ->  Signature.Closure(fnVal, [])
        | Var id -> 
            let (fnVal', types) = ID.Map.find id context.untyped_closures in 
            Signature.Closure (fnVal', types)
      
        | _ -> failwith "expected function!" 
      in   
       IFDEF DEBUG THEN Printf.printf "HIGHER ORDER 3!%!\n"; ENDIF;
      let signature = { 
        Signature.inputs = 
          fnSigElt :: (List.map (fun t -> Signature.Type t) types);
        outputs = None
      } 
      in 
      IFDEF DEBUG THEN Printf.printf "HIGHER ORDER 4!%!\n"; ENDIF;
      let specializedFn = 
        specialize_function_value context.interp_state fn.value signature 
      in
      IFDEF DEBUG THEN Printf.printf "HIGHER ORDER 5!%!\n"; ENDIF;
      let closureParams = match fnVal with 
        | Var id -> 
            if ID.Map.mem id context.closure_params then 
              ID.Map.find id context.closure_params
            else []
        | _ -> [] 
      in 
      IFDEF DEBUG THEN Printf.printf "HIGHER ORDER 6!%!\n"; ENDIF;
      let expNode = { 
        exp_src = expSrc;  
        exp_types = DynType.fn_output_types specializedFn.value_type; 
        exp = App(specializedFn, closureParams @ dataArgs') 
      }
      in 
      expNode, context, argsChanged
      
  (* for now assume all named functions are top-level and that none*)
  (* of the arguments are themselves functions. This means that for now *)
  (* the only higher order functions are built in primitives. *)  
  | GlobalFn _ 
  | Prim _ ->
      let args', types,  argsChanged = annotate_values context args in 
      let signature = Signature.from_input_types types in 
      let specializedFn = 
        specialize_function_value context.interp_state fn.value signature 
      in
      let expNode = {
        exp_src = expSrc;  
        exp_types = DynType.fn_output_types specializedFn.value_type; 
        exp = App(specializedFn, args') 
      }
      in 
      expNode, context, argsChanged               
  | Lam _ -> failwith "anonymous functions should have been named by now"
  | Var id ->
    IFDEF DEBUG THEN Printf.printf "ANNOTATE APP 2!%!\n"; ENDIF;
      (* ignore changes to args since we know we're definitely changing 
         this node to either a function call or array indexing 
      *)  
      let args', argTypes, _ = annotate_values context args in
      (* if the ID is an untyped closure, then 
         we need to make a specialized instance of it 
      *) 
      if ID.Map.mem id context.untyped_closures then (
        (* THIS IS ALL ONE TERRIBLE HACK *) 
        let (untypedFnVal, oldArgSigs) = 
          ID.Map.find  id context.untyped_closures 
        in
        let signature = { 
          Signature.inputs = oldArgSigs @ 
            (List.map (fun t -> Signature.Type t) argTypes);
          Signature.outputs = None; 
        } 
        in  
        IFDEF DEBUG THEN Printf.printf "ANNOTATE APP 3!%!\n"; ENDIF;
        let specializedFn = 
          specialize_function_value context.interp_state untypedFnVal signature 
        in
        let typedClosureId = ID.gen() in
        let closureMapping = context.typed_closure_mapping in 
        let closureSet = 
          if ID.Map.mem id closureMapping then  
            ID.Map.find id closureMapping
          else 
            ID.Set.empty 
        in  
        let closureSet' = ID.Set.add typedClosureId closureSet in 
        let closureMapping' = ID.Map.add id closureSet' closureMapping in   
        let oldTypes = Signature.sig_elts_to_types oldArgSigs in
        let outputTypes = DynType.fn_output_types specializedFn.value_type in 
        let typedClosureArg = 
          SSA.mk_var ~ty:(FnT(oldTypes, argTypes, outputTypes)) typedClosureId 
        in   
        let callNode = {
          exp=App(typedClosureArg, args'); 
          exp_types = outputTypes; 
          exp_src = None;  
        } 
        in
        let typedClosures = 
          ID.Map.add 
            typedClosureId 
            (specializedFn, argTypes, outputTypes) 
            context.typed_closures
        in   
        let context' = { context with 
          typed_closure_mapping = closureMapping';
          typed_closures = typedClosures; 
        }
        in 
        callNode, context', true       
        )
      else 
        let arrayType = ID.Map.find id context.type_env in
        let arrNode = { fn with value_type = arrayType } in  
        if DynType.is_scalar arrayType then 
          failwith "indexing requires lhs to be an array"
        ;  
      (* ignore changes to the args since we know we're definitely 
         changing this node from application of a Var to Prim.Index
      *)
      let args', argTypes, _ = annotate_values context args in
      let resultType = DynType.slice_type arrayType argTypes in
      let indexNode = { fn with 
        value_type = DynType.FnT(arrayType :: argTypes, [resultType]);
        value = Prim (Prim.ArrayOp Prim.Index); 
      } 
      in 
      let expNode = {  
        exp_src= expSrc;  
        exp_types = [resultType]; 
        exp = App(indexNode, arrNode :: args');
      }
      in 
      expNode, context, true
    
  | other -> failwith $ Printf.sprintf 
      "expected either a function or an array, received: %s" 
      (SSA.value_to_str other) 
           
and annotate_exp context expNode =
  IFDEF DEBUG THEN Printf.printf "ANNOTATE EXP 1!%!\n"; ENDIF;
  (* if the overall types returned by the expression change then *)
  (* this taken into account at the bottom of the function *)  
  let expNode', context', changed = match expNode.exp with 
  | Values vNodes ->
      IFDEF DEBUG THEN Printf.printf "ANNOTATE EXP->VALUES!%!\n"; ENDIF;
      let vNodes', vTypes, anyChanged = annotate_values context vNodes in
      let expNode' = { expNode with 
        exp= Values vNodes'; 
        exp_types = vTypes; 
      } 
      in 
      expNode', context, anyChanged  
  | Cast(castType, vNode) -> 
      IFDEF DEBUG THEN Printf.printf "ANNOTATE EXP->CAST!%!\n"; ENDIF;
      let vNode',  vType, changed = annotate_value context vNode in 
      let exp' = 
        if vType = castType then Values [vNode'] 
        else Cast(castType, vNode')
      in 
      let expNode' = { expNode with
        exp = exp';  
        exp_types = [castType] 
      } 
      in 
      expNode', context, changed
      
  | Arr vs ->
      IFDEF DEBUG THEN Printf.printf "ANNOTATE EXP->ARRAY!%!\n"; ENDIF; 
      let vs', types, anyValueChanged = annotate_values context vs in 
      begin match DynType.fold_type_list types with 
      | AnyT -> failwith "failed to find common type for elements of array"
      | commonT -> 
            (* all elements of array must be the same type *) 
            let vs'' = 
              List.map (fun v -> {v with value_type = commonT}) vs' 
            in
            let anyUpcast = 
              List.exists2 
                (fun vOld vNew -> vOld.value_type <> vNew.value_type)
                vs' vs'' in 
            let expNode' = 
              { expNode with exp_types = [VecT commonT]; exp = Arr vs'' } 
            in 
            expNode', context, anyValueChanged || anyUpcast
      end
  | App (fn, args) ->
     (* assume we're never getting too few arguments, 
        closure creation handle in annotate_stmt
      *)
       
     IFDEF DEBUG THEN
       Printf.printf "[annotate_exp] %s(%s)\n"
         (SSA.value_node_to_str fn)
         (SSA.value_node_list_to_str args)
       ;  
       let nargs = List.length args in 
       if nargs > max_arity context fn.value then 
         failwith "too many arguments"
     ENDIF; 
     annotate_app context expNode.exp_src fn args
  
  | ArrayIndex (arr, indices) -> 
        failwith "annotation of array indexing not yet supported"
  in 
  let typesChanged = expNode.exp_types <> expNode'.exp_types   in 
  expNode', context', changed || typesChanged 

and annotate_stmt context stmtNode = 
  let get_type id = 
    if ID.Map.mem id context.type_env 
    then ID.Map.find id context.type_env 
    else DynType.BottomT
  in  
  IFDEF DEBUG THEN 
    Printf.printf "[annotate_stmt] %s\n"
      (SSA.stmt_node_to_str stmtNode)
    ; 
  ENDIF;   
  match stmtNode.stmt with
  (* a under-applied function should form a closure 
     instead of getting specialized on the spot 
  *)  
  | Set([id], ({exp=App (fn, args)} as app)) 
    when 
    (not $ has_array_type context fn.value) && 
    (List.length args < min_arity context fn.value) ->
      (* can't annotate anything since the function isn't fully applied, 
         instead construct a signature string from the arguments 
      *) 
      IFDEF DEBUG THEN 
        Printf.printf "Partially applying %s = %s(%s)\n"
          (ID.to_str id)
          (SSA.value_to_str fn.value)
          (SSA.value_node_list_to_str args)
        ; 
      ENDIF; 
      let args, argTypes, _ = annotate_values context args in
      let argSig = value_nodes_to_sig_elts context args in
      let untypedClosures = context.untyped_closures in 
      let closure = match fn.value with 
        | Var closureId -> 
            IFDEF DEBUG THEN 
              assert (ID.Map.mem id untypedClosures); 
            ENDIF; 
            let (fnId, oldClosureArgs) = 
              ID.Map.find closureId untypedClosures 
            in 
            (fnId, oldClosureArgs @ argSig)
        | primOrGlobal -> 
            IFDEF DEBUG THEN
              let isFn = match primOrGlobal with 
                | Prim _ | GlobalFn _ -> true 
                | _ -> false
              in  
              assert isFn
            ENDIF;
            (primOrGlobal, argSig)
      in  
      (* TODO: figure out if we've already added this closure to env *) 
      let context'  = { context with 
        untyped_closures = ID.Map.add id closure untypedClosures; 
        closure_params = ID.Map.add id args context.closure_params; 
      }
      in 
      let stmtNode' = { stmtNode with 
        stmt = Set([id], {app with exp=App (fn, args)}); 
      } 
      in  
      stmtNode', context', true        
      
  | Set(ids, rhs) ->  
      let rhs', rhsContext, rhsChanged = annotate_exp context rhs in 
      let oldTypes = List.map get_type ids in   
      let newTypes = rhs'.exp_types in
      IFDEF DEBUG THEN  
        if List.length oldTypes <> List.length newTypes then 
          failwith $ 
            sprintf 
              "[annotate_stmt] malformed SET statement: \
               %d identifiers for %d rhs values \n"
               (List.length oldTypes)
               (List.length newTypes)
      ENDIF; 
      let commonTypes = List.map2 DynType.common_type oldTypes newTypes in
      IFDEF DEBUG THEN  
        if List.exists ((=) DynType.AnyT) commonTypes then  
          failwith "error during type inference"
        ; 
        assert (List.length ids = List.length newTypes);
      ENDIF; 
       
      let changed = rhsChanged || List.exists2 (<>) oldTypes newTypes in
      let context' = { rhsContext with 
        type_env = ID.Map.extend rhsContext.type_env ids newTypes
      } 
      in   
      let stmtNode' = { stmtNode with stmt = Set(ids, rhs') } in 
      stmtNode', context', changed 
  | SetIdx (id, indices, rhs) -> failwith "SetIdx stmt not yet implemented"
  | If (cond, tBlock, fBlock, gate) -> 
      let cond', _, condChanged = annotate_value context cond in
      let tBlock', tContext, tChanged = annotate_block context tBlock in 
      let fBlock', fContext, fChanged = annotate_block context fBlock in
      let stmtNode' = 
        {stmtNode with stmt = If(cond', tBlock', fBlock', gate)}
      in 
      let changed = condChanged || tChanged || fChanged in
      (* apply type join operator to any variables two type environments*)
      (* have in common *)
      let context' = 
        if changed then 
          merge_contexts 
            tContext
            gate.SSA.true_ids  
            fContext 
            gate.SSA.false_ids 
            context 
            gate.SSA.if_output_ids
        else context 
      in     
      stmtNode', context', changed 
          
and annotate_block context = function 
  | [] -> [], context, false 
  | stmtNode::rest -> 
        let stmtNode', context', currChanged = annotate_stmt context stmtNode in
        let rest', restContext, restChanged  = annotate_block context' rest in
        (stmtNode' :: rest'), restContext, currChanged || restChanged  


