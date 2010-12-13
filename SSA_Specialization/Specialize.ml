(* pp: -parser o pa_macro.cmo *)

open Base
open SSA
open SSA_Codegen 
open DynType
open Printf 

let rec specialize_value interpState fnVal signature =
  match InterpState.maybe_get_specialization interpState fnVal signature with
  | Some fnId -> InterpState.get_typed_function interpState fnId
  | None ->  
    let fundef', closureEnv =
      CollectPartialApps.collect_partial_apps interpState fundef 
    in
    (* to avoid having to make TypeAnalysis and Specialize recursive 
         modules I've untied the recursion by making specialize_value 
         a parameter of TypeAnalysis. 
    *)
    let tenv = 
      TypeAnalysis.type_analysis 
        interpState 
        (get_output_types interpState)
        closureEnv 
        fundef'
        signature 
    in 
    let body', tenv' = InsertCoercions.rewrite_block tenv fundef'.body in
    let typedFundef = 
      mk_fundef 
        ~tenv:tenv' 
        ~body:body'
        ~input_ids:fundef.input_ids 
        ~output_ids:fundef.output_ids
    in      
    InterpState.add_specialization fnVal signature typedFn;  
    typedFn             
and get_output_types interpState fnVal signature = 
  let fundef = specialize_val fnVal signature in 
  fundef.fundef_output_types 
  

(*
type type_env = DynType.t ID.Map.t
type const_env = SSA.value ID.Map.t 
type untyped_closure_env = (SSA.value * Signature.sig_elt list) ID.Map.t 
type typed_closure_env = 
       (SSA.value_node * DynType.t list * DynType.t list) ID.Map.t


type context = {
  type_env : type_env; 
  const_env : const_env; 
  typed_closures : typed_closure_env; 
  untyped_closures : untyped_closure_env;
  (* map every untyped closure to its partial list of typed params *) 
  closure_params : SSA.value_node list ID.Map.t;  
  (* map untyped closure id to typed variant *) 
  typed_closure_mapping  : ID.Set.t ID.Map.t;
  interp_state : InterpState.t 
}  

let get_type context id = 
  if ID.Map.mem id context.type_env 
  then ID.Map.find id context.type_env 
  else DynType.BottomT

let combine_option left right = match left, right with 
    | None, Some c -> Some c 
    | Some c, None -> Some c 
    | Some c1, Some c2 ->  
        if c1 <> c2 then 
          failwith "expected left and right values to be disjoint"
        else Some c1
    | None, None -> None 

(* 
   generic merge function which takes two branch lookup functions, an 
   initial merged environment 
   and corresponding lists of ids on the left, ids on right 
   and their names in the merged environment.  
*) 
let rec merge_ssa_flows
         (combineOp : 'b -> 'b -> 'a option)
         (lookup1 : ID.t -> 'b)
         (ids1 : ID.t list)
         (lookup2 : ID.t -> 'b)
         (ids2 : ID.t list)
         (outputEnv : 'a ID.Map.t)
         (outputIds : ID.t list) = 
  match ids1, ids2, outputIds with 
    | [], [] , [] -> outputEnv 
    | id1::rest1, id2::rest2, outId::restOut -> 
        let t1 = lookup1 id1 in 
        let t2 = lookup2 id2 in
        let outputEnv' = match combineOp t1 t2 with
          | Some t3 -> ID.Map.add outId t3 outputEnv  
          | None -> outputEnv 
        in  
        merge_ssa_flows combineOp lookup1 rest1 lookup2 rest2 outputEnv' restOut 
   | _ -> failwith "length mismatch while combining type environments"     

let combine_single_entry 
      (id: ID.t)
      (mergeOp : 'a -> 'a -> 'a)
      (env1 : 'a ID.Map.t)
      (env2 : 'a ID.Map.t) : 'a option = 
    match ID.Map.find_option id env1, ID.Map.find_option id env2 with
      | Some t1, Some t2 -> Some (mergeOp t1 t2) 
      | None, Some t 
      | Some t, None -> Some t 
      | None, None -> None 
   
let combine_all_entries 
      (mergeOp : 'a -> 'a -> 'a)
      (env1 : 'a ID.Map.t)
      (env2 : 'a ID.Map.t) : 'a ID.Map.t =
  let keys1 = ID.Map.key_set env1 in 
  let keys2 = ID.Map.key_set env2 in 
  let allKeys = ID.Set.union keys1 keys2 in
  ID.Set.fold 
    (fun id accMap ->
        match combine_single_entry id mergeOp env1 env2 with 
          | None -> accMap 
          | Some t -> ID.Map.add id t accMap   
    )
    allKeys 
    ID.Map.empty 

let fail_on_merge _ _ = failwith "can't merge"
    
let merge_contexts c1 ids1 c2 ids2 outputCxt outputIds =
  let tenv = 
    merge_ssa_flows 
      (fun t1 t2 -> Some (DynType.common_type t1 t2)) 
      (fun id -> ID.Map.find id c1.type_env) ids1  
      (fun id -> ID.Map.find id c2.type_env) ids2 
      outputCxt.type_env 
      outputIds
  in 
  let lookupClosure cEnv id = ID.Map.find_option id cEnv in
  let typedClosures = 
    combine_all_entries fail_on_merge c1.typed_closures  c2.typed_closures
  in 
  let untypedClosures =  
    combine_all_entries fail_on_merge c1.untyped_closures c2.untyped_closures 
  in 
  let closureParams = 
    combine_all_entries fail_on_merge c1.closure_params c2.closure_params
  in 
  let closureMapping =
    combine_all_entries 
      ID.Set.union
      c1.typed_closure_mapping
      c2.typed_closure_mapping 
  in 
  { outputCxt with 
      type_env = tenv;
      typed_closures = typedClosures; 
      untyped_closures = untypedClosures; 
      typed_closure_mapping = closureMapping;
      closure_params = closureParams; 
  }     
     
    

type change_indicator = bool    
  
let rec min_arity context = function 
  | Prim op -> Prim.min_prim_arity op
  | GlobalFn fnId -> InterpState.get_untyped_arity context.interp_state fnId
  | Var closureId ->
      if ID.Map.mem closureId context.untyped_closures then 
        let fnVal, closureSig = 
          ID.Map.find closureId context.untyped_closures 
        in
        let nClosureArgs = List.length closureSig in
        IFDEF DEBUG THEN Printf.printf "NUM CLOSURE ARGS: %d\n" nClosureArgs; ENDIF; 
        let fullArity = min_arity context fnVal in  
        
        fullArity - nClosureArgs
      else 7770   
  | other -> failwith $ 
     Printf.sprintf 
       "Can't get arity of %s" 
       (SSA.value_to_str other)
      
let max_arity context = function 
  | Prim op -> Prim.max_prim_arity op 
  | other -> min_arity context other 
    
let sig_from_type = function 
  | FnT([], inTypes, outTypes) -> Signature.from_types inTypes outTypes
  | FnT(_::_, _, _) -> failwith "closures not yet supported" 
  | _ -> failwith "expected function type"

(* TODO: make this complete for all SSA statements *) 
let rec is_scalar_stmt = function
  | SSA.Set(_, {exp=SSA.App({value=SSA.Prim (Prim.ScalarOp _)}, _)})
  | SSA.Set(_, {exp=Values _}) -> true 
  | SSA.If(_, tCode, fCode, _) -> 
      all_scalar_stmts tCode && all_scalar_stmts fCode
  | _ -> false   
 
and all_scalar_stmts = function 
  | [] -> true
  | {stmt=stmt}::rest -> is_scalar_stmt stmt &&  all_scalar_stmts rest

let type_of_raw_value = function 
  | Num n -> PQNum.type_of_num n 
  | Str _ -> DynType.StrT 
  | Sym _ -> DynType.SymT  
  | Unit -> DynType.UnitT
  | _ -> assert false  

let has_array_type context = function 
  | Var id -> 
      let tenv = context.type_env in 
      ID.Map.mem id tenv && 
      DynType.is_vec (ID.Map.find id tenv)
  | _ -> false  


(*
(* instead of returning the type of a value, return its sig_elt *) 
let annotate_value_sig context valNode = 
  match valNode.value with 
  | Var id -> 
    let t  
    ID.Map.find id context.type_env  
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
*)
  


let annotate_value context valNode = 
  IFDEF DEBUG THEN Printf.printf "ANNOTATE VALUE !%!\n"; ENDIF;
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


let rec annotate_values context = function 
  | [] -> [], [], false  
  | v::vs ->
      let v', t, currChanged = annotate_value context v in 
      let vs', ts, restChanged = annotate_values context vs in 
      v'::vs', t::ts, currChanged || restChanged 

let value_to_sig_elt context v : Signature.sig_elt = 
  IFDEF DEBUG THEN Printf.printf "VALUE_TO_SIG !%!\n"; ENDIF;
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
   IFDEF DEBUG THEN Printf.printf "SPECIALIZE_SCALAR_PRIM !%!\n"; ENDIF;
  let expectedTypes = TypeInfer.required_scalar_op_types op inputTypes in 
  let inferredOutputT = TypeInfer.infer_scalar_op op inputTypes in
  let typedPrim = { 
    value = Prim (Prim.ScalarOp op); 
    value_type = DynType.FnT([], expectedTypes, [inferredOutputT]); 
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
        value_type = DynType.FnT([], arrayType :: argTypes, [resultType]);
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

(* make a scalar version of a function whose body contains only 
   potentially scalar operators, and wrap this function in a map over
   1D vector data 
*)
and scalarize_fundef interpState untypedId untypedFundef vecSig = 
  let scalarSig = Signature.peel_vec_types vecSig in 
  let scalarFundef = specialize_fundef interpState untypedFundef scalarSig in
  InterpState.add_specialization interpState 
    (GlobalFn untypedId) 
    scalarSig 
    scalarFundef 
  ;
  let scalarOutputTypes = DynType.fn_output_types scalarFundef.fn_type in  
  let inputTypes = Signature.input_types vecSig in  
  let outputTypes = List.map (fun t -> VecT t) scalarOutputTypes in
  let freshInputIds = List.map (fun _ -> ID.gen()) untypedFundef.input_ids in
  let freshOutputIds = List.map (fun _ -> ID.gen()) untypedFundef.output_ids in
  let extend_env accEnv id t = ID.Map.add id t accEnv in 
  IFDEF DEBUG THEN 
    assert (List.length freshInputIds = List.length inputTypes);
    assert (List.length freshOutputIds = List.length outputTypes);  
  ENDIF; 
  let inputTyEnv = 
    List.fold_left2 extend_env ID.Map.empty freshInputIds inputTypes  
  in 
  let combinedTyEnv = 
    List.fold_left2 extend_env inputTyEnv freshOutputIds outputTypes
  in
  let scalarFnType = scalarFundef.fn_type in  
  let mapType = FnT([], scalarFnType :: inputTypes, outputTypes) in  
  let mapNode = SSA.mk_val ~ty:mapType (SSA.Prim (Prim.ArrayOp Prim.Map)) in
  let fnNode = SSA.mk_val ~ty:scalarFnType (GlobalFn scalarFundef.fundef_id) in     
  let dataNodes =
    List.map2 (fun id t -> SSA.mk_var ~ty:t id) freshInputIds inputTypes
  in
  let argNodes = fnNode :: dataNodes in   
  let appNode = SSA.mk_app ~types:outputTypes mapNode argNodes in
  SSA.mk_fundef 
    ~body:[SSA.mk_set freshOutputIds appNode]      
    ~tenv:combinedTyEnv 
    ~input_ids:freshInputIds
    ~output_ids:freshOutputIds  

and specialize_function_id interpState untypedId signature =
  match 
    InterpState.maybe_get_specialization 
      interpState 
      (GlobalFn untypedId) 
      signature 
  with 
    | None ->
       IFDEF DEBUG THEN 
         Printf.printf "[specialize_function_id] Specializing fn%d for %s\n"
           untypedId
           (Signature.to_str signature)
         ;
          
       ENDIF; 
       let untypedFundef = 
         InterpState.get_untyped_function interpState untypedId 
       in 
     (* SPECIAL CASE OPTIMIZATION: if we see a function of all scalar operators
        applied to all 1D vectors, then we can directly generate a single 
        Map adverb over all the argument vectors 
     *)
       let inputTypes = Signature.input_types signature in
       let typedFundef = 
         if all_scalar_stmts untypedFundef.body  
            && List.for_all DynType.is_scalar_or_vec inputTypes 
            && List.exists DynType.is_vec inputTypes 
         then scalarize_fundef interpState untypedId untypedFundef signature 
         else specialize_fundef interpState untypedFundef signature
       in
       InterpState.add_specialization 
           interpState 
           (GlobalFn untypedId) 
           signature 
           typedFundef
       ;
       typedFundef   
    | Some typedId -> InterpState.get_typed_function interpState typedId 
          
and specialize_fundef interpState untypedFundef signature = 
  let rec add_sig_elts tyEnv constEnv closureEnv ids elts = 
    match (ids, elts) with 
    | [],[] -> tyEnv, constEnv, closureEnv
    | _, [] | [], _ -> failwith "length mismatch while processing sig elts" 
    | id::ids, (Signature.Type t)::rest ->
        let tyEnv' = ID.Map.add id t tyEnv in 
        add_sig_elts tyEnv' constEnv closureEnv ids rest   
    | id::ids, (Signature.Const n)::rest ->
        let constEnv' = ID.Map.add id (SSA.Num n) constEnv in  
        add_sig_elts tyEnv constEnv' closureEnv ids rest
    | id::ids, (Signature.Closure (fnVal, args))::rest  ->
        let closureEnv' = ID.Map.add id (fnVal, args) closureEnv in  
        add_sig_elts tyEnv constEnv closureEnv' ids rest 
  in  
  IFDEF DEBUG THEN
    let nActual = List.length untypedFundef.input_ids in 
    let nExpected = List.length signature.Signature.inputs in 
    if nActual <> nExpected then failwith $ Printf.sprintf 
      "mismatch between fn arity: actual (%d), expected (%d) for %s : %s"
      nActual 
      nExpected 
      (FnId.to_str untypedFundef.fundef_id) 
      (Signature.to_str signature)
    ;  
  ENDIF;  
  let tyEnv, constEnv, untypedClosureEnv = 
    add_sig_elts 
      ID.Map.empty 
      ID.Map.empty 
      ID.Map.empty 
      untypedFundef.input_ids 
      signature.Signature.inputs      
  in 
  let context = { 
    type_env = tyEnv; 
    const_env = constEnv;
    untyped_closures = untypedClosureEnv;
    typed_closures = ID.Map.empty;
    typed_closure_mapping = ID.Map.empty;
    closure_params = ID.Map.empty;       
    interp_state = interpState
  }
  in
  let annotatedBody, context, _ = annotate_block context untypedFundef.body in
  (* annotation returned a mapping of all identifiers to their types, *)
  (* now use this type info (along with annotatins on values) to insert all *)
  (* necessary coercions and convert untyped to typed representation. *) 

  let typedClosureBody = 
    InsertTypedClosures.process_block 
      context.typed_closure_mapping
      context.typed_closures
      annotatedBody
  in  
  (* returns both a typed body and an environment which includes bindings *)
  (* for any temporaries introduced from coercions *)  
   
  let typedBody, finalTyEnv = 
    InsertCoercions.rewrite_block context.type_env typedClosureBody 
  in
  SSA.mk_fundef 
    ~body:typedBody 
    ~tenv:finalTyEnv
    ~input_ids:untypedFundef.input_ids
    ~output_ids:untypedFundef.output_ids
  
  
(****************************************************************************)
(*              SPECIALIZE FUNCTION VALUE                                   *)
(****************************************************************************)
and specialize_function_value interpState v signature : SSA.value_node = 
  IFDEF DEBUG THEN Printf.printf "SPECIALIZE FUNCTION VALUE!%!\n"; ENDIF;
  match InterpState.maybe_get_specialization interpState v signature with 
    | Some fnId -> 
        IFDEF DEBUG THEN
          Printf.printf "Found %s for %s : %s" 
            (FnId.to_str fnId)
            (SSA.value_to_str v)
            (Signature.to_str signature)
          ;
        ENDIF; 
        let fundef =  InterpState.get_typed_function interpState fnId in 
        { value = GlobalFn fnId; 
          value_type = fundef.fn_type; 
          value_src = None 
        }
           
    | None -> 
      IFDEF DEBUG THEN Printf.printf "SPECIALIZE FUNCTION VALUE 2!%!\n"; ENDIF;
      let typedFundef = match v with
        (* specialize the untyped function -- assuming it is one *) 
      | GlobalFn untypedId  ->
         IFDEF DEBUG THEN 
           Printf.printf 
             "[specialize_function_value] GlobalFn %s w/ sig %s\n"
             (SSA.value_to_str v)
             (Signature.to_str signature);
         ENDIF;  
         specialize_function_id interpState untypedId signature
         
      (* first handle the simple case when it's a scalar op with scalar args *)
      | Prim (Prim.ScalarOp op) when 
          List.for_all DynType.is_scalar (Signature.input_types signature) ->
        specialize_scalar_prim_for_scalar_args
          interpState 
          op 
          ?forceOutputType:(Option.map List.hd signature.Signature.outputs) 
          (Signature.input_types signature)
      | Prim (Prim.ScalarOp op) when 
         (* not scalar args *)
         List.for_all 
           (fun t -> DynType.is_scalar t || DynType.is_vec t)
           (Signature.input_types signature)  -> 
        specialize_scalar_prim_for_vec_args
          interpState 
          op
          ?forceOutputType:(Option.map List.hd signature.Signature.outputs) 
          (Signature.input_types signature)
                   
      | Prim (Prim.ArrayOp op) when Prim.is_higher_order op ->
        (match signature.Signature.inputs with 
          | Signature.Closure (fnVal, closureArgSig)::restSig ->
            (* after the function value all the other arguments should *)
            (* be data to which we can assign types *) 
            IFDEF DEBUG THEN 
              Printf.printf "[specialize_function_value] %s : {%s} => %s  \n"
                (Prim.array_op_to_str op)
                (Signature.sig_elts_to_str closureArgSig)
                (SSA.value_to_str fnVal)
              ; 
            ENDIF; 
                
            let inputTypes = Signature.sig_elts_to_types restSig in 
            specialize_higher_order_array_prim 
              interpState
              op 
              [fnVal, closureArgSig]
              ?forceOutputTypes:signature.Signature.outputs
              inputTypes

          | others -> 
              failwith "expected function value for higher-order array operator"
        )
        
      (* array operators which don't take function arguments *) 
      | Prim (Prim.ArrayOp op) -> 
          specialize_first_order_array_prim 
            interpState 
            op 
            ?forceOutputTypes:signature.Signature.outputs 
            (Signature.input_types signature)
      
      (* make some sense of Q's highly ambiguous overloaded operators *) 
      | Prim (Prim.Q_Op qOp) -> 
          specialize_q_operator
            interpState 
            qOp
            ?forceOutputTypes:signature.Signature.outputs
            (Signature.input_types signature)
      | other -> 
          failwith $ Printf.sprintf 
            "invalid value type for specialize_function_value: %s"
            (SSA.value_to_str other)
     in 
     InterpState.add_specialization interpState v signature  typedFundef; 
     { value = GlobalFn typedFundef.fundef_id; 
       value_type = typedFundef.fn_type; 
       value_src = None 
     }      

  (* if we're applying a scalar operator to at least one vector, transform *)
  (* the whole operation into a map *) 
and specialize_scalar_prim_for_vec_args 
      interpState 
      op 
      ?forceOutputType 
      inputTypes : SSA.fundef =
    IFDEF DEBUG THEN 
      Printf.printf "SPECIALIZE SCALAR PRIM FOR VEC ARGS: %s !%!\n"
        (DynType.type_list_to_str inputTypes)
      ;
    ENDIF;  
    let eltTypes = List.map DynType.elt_type inputTypes in
    let nestedSig = {
      Signature.inputs = List.map (fun t -> Signature.Type t) eltTypes; 
      outputs = None
    } 
    in 
    let nestedFn = 
      specialize_function_value interpState (Prim (Prim.ScalarOp op)) nestedSig  
    in
    let nestedOutputTypes = DynType.fn_output_types nestedFn.value_type in
    (* the final output will be the nested output wrapped in a layer of VecT *)
    let outputTypes = List.map (fun t -> VecT t) nestedOutputTypes in    
    (* this map node takes the nestedFn, and the argument types to *) 
    (* produce the output types *) 
    let mapNode = { 
      value = Prim (Prim.ArrayOp Prim.Map);
      value_type = FnT([], nestedFn.value_type::inputTypes, outputTypes); 
      value_src = None 
    }   
    in
    (* create a function definition whose body maps over the inputs *)    
    mk_lambda inputTypes outputTypes 
        (fun codegen inputs outputs ->
            let appNode = 
              mk_app ~types:outputTypes mapNode (nestedFn::inputs) 
            in 
            codegen#emit [mk_set (get_ids outputs) appNode]
        ) 
  
(* create an anonymous function which performs a typed map of the given *)
(* function f over arguments of type 'inputTypes' and returns a value of*)
(* either the inferred types or of 'forceOutputTypes' if provided. *) 
and specialize_map 
    interpState
    (f : value) 
    ?(forceOutputTypes : DynType.t list option) 
    (closureTypes : DynType.t list)
    (inputTypes :DynType.t list)
    : SSA.fundef  =
  IFDEF DEBUG THEN 
    Printf.printf "[specialize_map] %s : %s with inputs %s\n"
      (SSA.value_to_str  f)
      (DynType.type_list_to_str inputTypes)
    ;
  ENDIF; 
  let eltTypes =  List.map DynType.peel_vec inputTypes in 
  (* if the outputs need to be coerced, pass on that information to the *)
  (* nested function so that coercions get inserted at the scalar level.*)
  (* For example, if we have "map {x*x} 1 2 3" : float vec, it's preferable*)
  (* to specialize into " map {[x] y = cast(x,float); y*y} 1 2 3" *)
  (* rather than:  "cast(map {x*x} 1 2 3, float vec)" *)
  let forceOutputEltTypes : DynType.t list option = 
    Option.map (List.map DynType.peel_vec) forceOutputTypes 
  in
  let nestedSig = 
    { (Signature.from_input_types (closureTypes @ eltTypes)) with 
       Signature.outputs = forceOutputEltTypes }
  in 
  let nestedFn = specialize_function_value interpState f nestedSig in
  let nestedOutputTypes = DynType.fn_output_types nestedFn.value_type in 
  let outputTypes = 
    TypeInfer.infer_adverb Prim.Map (nestedFn.value_type::inputTypes) 
  in 
  let fnArgType = DynType.FnT(closureTypes, eltTypes, nestedOutputTypes) in 
  let mapNode = { 
    value = Prim (Prim.ArrayOp Prim.Map);
    value_type = DynType.FnT([], fnArgType::inputTypes, outputTypes); 
    value_src = None
  } 
  in
  let nClosureArgs = List.length closureTypes in 

  SSA_Codegen.mk_lambda (closureTypes @ inputTypes) outputTypes 
    (fun codegen inputs outputs ->
      IFDEF DEBUG THEN 
        Printf.printf 
          "Generating MAP body with %d closure args and %d total inputs\n"
           (List.length closureTypes) (List.length inputs)
        ; 
      ENDIF; 
      let closureVar = 
        if nClosureArgs > 0 then (
          let closureT = 
            DynType.FnT(closureTypes, eltTypes, nestedOutputTypes)
          in   
          let mkClosureNode = {
            exp = App(nestedFn, List.take nClosureArgs inputs); 
            exp_types = [closureT];
            exp_src = None; 
          } 
          in 
          let closId = codegen#fresh_var closureT in
          let closVar = 
            {value=Var closId; value_type=closureT; value_src=None}
          in  
          codegen#emit[mk_set [closId] mkClosureNode]; 
          closVar 
        )
        else nestedFn 
      in  
      let appNode = { 
        exp = App(mapNode, closureVar::(List.drop nClosureArgs inputs)); 
        exp_types = outputTypes; 
        exp_src = None; 
      } 
      in 
      codegen#emit [mk_set (get_ids outputs) appNode]
    ) 

(* specialize a reduction which starts with a base elemenet of type baseType 
 and then reduces the vector data (of types vecTypes) using function f to 
 produce consecutive values. 
 
 POTENTIAL PROBLEM: what if  the baseType doesn't match the return type. 
 For example, 0 +/ [1.0 2.0] will initially specialize +:int,float -> float 
 but once the intermediate value has become a float we need +:float,float->float
 For now we can simply specialize the reduction function twice if necessary and 
 cast the intermediate value to the return value of f. 
 Rough type rule for reducing a single vector - 
    REDUCE(f, i, xs) : (a, b -> a), c <: a, b vec -> a vec 
*)   
and specialize_reduce interpState f ?forceOutputTypes baseType vecTypes 
        : SSA.fundef =
  let forceOutputEltTypes = 
    Option.map (List.map DynType.peel_vec ) forceOutputTypes 
  in
  
  let nestedVecTypes = List.map DynType.peel_vec vecTypes in  
  let nestedSig = {
      Signature.inputs = 
        List.map (fun t -> Signature.Type t) (baseType::nestedVecTypes); 
      outputs = forceOutputEltTypes; 
  } 
  in
     
  let nestedFn = specialize_function_value interpState f nestedSig in
  
  (* have to specialize twice in case output of reduction function doesn't *)
  (* match baseType *)
  let outputTypes =  DynType.fn_output_types nestedFn.value_type in 
  let fnNode = match outputTypes with 
    | [t] ->
      IFDEF DEBUG THEN 
        Printf.printf "[specialize] %s=?%s!\n"
          (DynType.to_str t)
          (DynType.to_str baseType)
        ;
      ENDIF;
      if t = baseType then nestedFn
      else    
        let nestedSig' = { 
          Signature.inputs = 
            List.map (fun t -> Signature.Type t) (t::nestedVecTypes);  
            Signature.outputs = Some [t] 
        } 
        in 
        specialize_function_value interpState f nestedSig' 
    | [] -> failwith "reduction function with 0 outputs not supported"
    | _ -> failwith "reduction function with multiple outputs not supported"
  in
  IFDEF DEBUG THEN Printf.printf "[specialize_reduce] 3\n"; ENDIF; 
  let accType = List.hd outputTypes in
  let inputTypes = [fnNode.value_type; accType]@ vecTypes in 
  let outputTypes = [accType] in    
  let reduceType = 
    DynType.FnT([], inputTypes, outputTypes)
  in 
  let reduceNode = 
    { value=Prim (Prim.ArrayOp Prim.Reduce); 
      value_type = reduceType;  
      value_src = None 
    }
  in 
  mk_lambda (baseType::vecTypes) outputTypes
   (fun codegen inputs outputs -> 
      let baseVal = List.hd inputs in 
      
      let baseVal' = 
         codegen#cvt ~from_type:baseType ~to_type:accType baseVal in
      
      let appNode = 
      { exp = App(reduceNode, fnNode :: baseVal' :: (List.tl inputs));  
        exp_types = [accType]; 
        exp_src = None;
      }
      in   
      codegen#emit [mk_set (get_ids outputs) appNode] 
   )      


(* create an anonymous function which performs a typed all-pairs of the given *)
(* function f over arguments of type 'inputType1' and 'inputType2' *) 
(* and returns values of either the inferred types *)
(* or of 'forceOutputTypes' if provided. *) 
and specialize_all_pairs 
    interpState 
    (f : value) 
    ?(forceOutputTypes : DynType.t list option) 
    (inputType1 : DynType.t)
    (inputType2 : DynType.t)
    : SSA.fundef  = 
  IFDEF DEBUG THEN Printf.printf "ALLPAIRS 1!%!\n"; ENDIF; 
  let arrayTypes = [inputType1; inputType2] in    
  let eltTypes = List.map DynType.peel_vec arrayTypes in   
  IFDEF DEBUG THEN Printf.printf "ALLPAIRS 2!%!\n"; ENDIF;
  let forceOutputEltTypes : DynType.t list option = 
    Option.map (List.map DynType.peel_vec) forceOutputTypes 
  in
  IFDEF DEBUG THEN Printf.printf "ALLPAIRS 3!%!\n"; ENDIF;
  let nestedSig = 
    { (Signature.from_input_types eltTypes ) with 
       Signature.outputs = forceOutputEltTypes }
  in 
  IFDEF DEBUG THEN Printf.printf "ALLPAIRS 4!%!\n"; ENDIF;
  let nestedFn = specialize_function_value interpState f nestedSig in
  IFDEF DEBUG THEN Printf.printf "ALLPAIRS 5!%!\n"; ENDIF;
  let inputTypes = nestedFn.value_type :: arrayTypes in 
  let outputTypes = TypeInfer.infer_adverb Prim.AllPairs inputTypes in
  IFDEF DEBUG THEN Printf.printf "ALLPAIRS 6!%!\n"; ENDIF;  
  let allPairsNode = { 
    value = Prim (Prim.ArrayOp Prim.AllPairs);
    value_type = DynType.FnT([], inputTypes, outputTypes ); 
    value_src = None
  } 
  in
  IFDEF DEBUG THEN Printf.printf "ALLPAIRS 7!%!\n"; ENDIF;
  SSA_Codegen.mk_lambda arrayTypes outputTypes 
    (fun codegen inputs outputs -> 
      IFDEF DEBUG THEN Printf.printf "ALLPAIRS 8!%!\n"; ENDIF;
      let appNode = { 
        exp = App(allPairsNode, nestedFn::inputs); 
        exp_types = outputTypes; 
        exp_src = None; 
      } 
      in 
      codegen#emit [mk_set (get_ids outputs) appNode];
      IFDEF DEBUG THEN Printf.printf "ALLPAIRS 9!%!\n"; ENDIF;
    ) 
            
and specialize_higher_order_array_prim 
    ( interpState : InterpState.t ) 
    ( op  : Prim.array_op ) 
    ( fnVals : (value * Signature.sig_elt list) list )  
    ?( forceOutputTypes : DynType.t list option ) 
    ( inputTypes : DynType.t list)
   : SSA.fundef  =  
  
  match op, fnVals, inputTypes with 
  | Prim.Map, [f,c], _ ->
    let closureArgTypes = Signature.sig_elts_to_types c in  
    specialize_map interpState f ?forceOutputTypes closureArgTypes inputTypes
  (* for reduction, split the type of the initial value from the vectors *)
  (* being reduced *)  
  | Prim.Reduce,  [f,c], (initType::vecTypes) ->
    let closureArgTypes = Signature.sig_elts_to_types c in   
    specialize_reduce interpState f ?forceOutputTypes initType vecTypes
  | Prim.AllPairs, [f,c], [t1; t2] -> 
    let closureArgTypes = Signature.sig_elts_to_types c in  
    specialize_all_pairs interpState f ?forceOutputTypes t1 t2 
  | op, _, types -> failwith $ Printf.sprintf 
      "[specialize] array operator %s not yet implemented for input of type %s"
      (Prim.array_op_to_str op)
      (DynType.type_list_to_str types) 

and specialize_first_order_array_prim 
    ( interpState : InterpState.t )
    ( op : Prim.array_op )
    ?( forceOutputTypes : DynType.t list option )
    ( inputTypes : DynType.t list) 
    : SSA.fundef =
  if forceOutputTypes <> None then 
     failwith  
     "[Specialize->specialize_first_order_array_op] \ 
      prespecified output types not implemented"
  else
  let outputType = TypeInfer.infer_simple_array_op op inputTypes in
  let arrayOpNode = { 
   value = Prim (Prim.ArrayOp op);
   value_type = DynType.FnT([], inputTypes, [outputType]); 
   value_src = None
  } 
  in 
  SSA_Codegen.mk_lambda inputTypes [outputType]
    (fun codegen inputs outputs -> 
       let appNode = { 
        exp = App(arrayOpNode, inputs); 
        exp_types = [outputType]; 
        exp_src = None; 
      } 
      in 
      codegen#emit [mk_set (get_ids outputs) appNode]
    )
    
and specialize_q_operator 
    ( interpState : InterpState.t )
    ( op : Prim.q_op )
    ?( forceOutputTypes : DynType.t list option )
    ( inputTypes : DynType.t list) =
  if forceOutputTypes <> None then 
     failwith $ 
     "[Specialize->specialize_q_op] prespecified output types not implemented"
 else
  match op, inputTypes with 
    | Prim.Q_Question, [left; right] 
      when DynType.is_vec left && DynType.is_scalar right -> 
         specialize_first_order_array_prim  interpState Prim.Find inputTypes  
    | _ ->  failwith "Q's not welcome here"

*)