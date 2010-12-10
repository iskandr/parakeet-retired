(* pp: -parser o pa_macro.cmo *)

open Base
open SSA
open SSA_Codegen 
open DynType
open Printf 


let rec specialize interpState fundef signature =
  match 
    InterpState.maybe_get_specialization 
      interpState 
      (GlobalFn fundef.fundef_id) 
      signature 
  with
  | None ->  
    let module R = struct
      (* to avoid having to make TypeAnalysis and Specialize recursive 
         modules I've untied the recursion by making the specialize function 
         a parameter of TypeAnalysis. 
      *) 
      let specialize = (specialize interpState)
      let interpState = interpState
    end 
    in 
    let module TypeEval = SSA_Analysis.MakeEvaluator(TypeAnalysis.Make(R)) in
    let context = TypeEval.eval_fundef fundef in 
    let body', tenv' = 
      InsertCoercions.rewrite_block context.TypeAnalysis.type_env fundef.body 
    in 
    { fundef with body = body'; tenv = tenv' }            
  | Some fnId -> InterpState.get_typed_function interpState fnId  

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
  let mapType = FnT(scalarFnType :: inputTypes, outputTypes) in  
  let mapNode = SSA.mk_val ~ty:mapType (SSA.Prim (Prim.ArrayOp Prim.Map)) in
  let fnNode = SSA.mk_val ~ty:scalarFnType (GlobalFn scalarFundef.fn_id) in     
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
      (FnId.to_str untypedFundef.fn_id) 
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
     { value = GlobalFn typedFundef.fn_id; 
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
      value_type = FnT(nestedFn.value_type::inputTypes, outputTypes); 
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
    value_type = DynType.FnT(fnArgType::inputTypes, outputTypes); 
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
    DynType.FnT(inputTypes, outputTypes)
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
    value_type = DynType.FnT(inputTypes, outputTypes ); 
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
   value_type = DynType.FnT(inputTypes, [outputType]); 
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
            