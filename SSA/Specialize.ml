(* pp: -parser o pa_macro.cmo *)

open Base
open SSA
open SSA_Codegen 
open DynType
open Printf 

type type_env = DynType.t ID.Map.t
type const_env = SSA.value ID.Map.t 

type context = {
  type_env : type_env; 
  const_env : const_env; 
  interp_state : InterpState.t 
}  

type change_indicator = bool    
  
let sig_from_type = function 
  | FnT(inTypes, outTypes) -> Signature.from_types inTypes outTypes 
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

let annotate_value context valNode = 
  let t = match valNode.value with 
  | Var id -> ID.Map.find id context.type_env  
  | Num n -> PQNum.type_of_num n 
  | Str _ -> DynType.StrT 
  | Sym _ -> DynType.SymT  
  | Unit -> DynType.UnitT
  | Prim _ -> 
      failwith "[Specialize] cannot annotate primitive without its arguments"
  | GlobalFn _ 
  | Lam _ -> 
      failwith "[Specialize] cannot annotate a function without its arguments" 
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
     
let rec annotate_app context expSrc fn args = match fn.value with
  (* for now assume that all higher order primitives take a single *)
  (* function argument which is passed first *)  
  | Prim (Prim.ArrayOp p) when Prim.is_higher_order p -> 
    assert (List.length args > 1); 
    let fnArg = List.hd args in 
    let dataArgs = List.tl args in 
    let dataArgs', types, argsChanged = annotate_values context dataArgs in
    (* for now just pass through the literal value but should eventually*)
    (* have a context available with an environment of literal value *)
    (* arguments and check whether the fnArg is in that environment. *)  
    let signature = { 
      Signature.inputs = 
        (Signature.Value fnArg.value) 
        :: (List.map (fun t -> Signature.Type t) types);
      outputs = None
    } 
    in 
    let specializedFn = 
      specialize_function_value context.interp_state fn.value signature 
    in
    let expNode = { 
      exp_src = expSrc;  
      exp_types = DynType.fn_output_types specializedFn.value_type; 
      exp = App(specializedFn, dataArgs') 
    }
    in expNode, argsChanged
      
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
    in expNode, argsChanged               
  | Lam _ -> failwith "anonymous functions should have been named by now"
  | Var arrayId -> 
    (* assume we're conflating array indexing and function application. *)
    let arrayType = ID.Map.find arrayId context.type_env in
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
    in expNode, true
    
  | other -> failwith $ Printf.sprintf 
      "expected either a function or an array, received: %s" 
      (SSA.value_to_str other) 
           
and annotate_exp context expNode =
  (* if the overall types returned by the expression change then *)
  (* this taken into account at the bottom of the function *)  
  let expNode', changed = match expNode.exp with 
  | Values vNodes ->
      let vNodes', vTypes, anyChanged = annotate_values context vNodes in
      let expNode' = { expNode with 
        exp= Values vNodes'; 
        exp_types = vTypes; 
      } 
      in 
      expNode', anyChanged  
  | Cast(castType, vNode) -> 
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
      expNode', changed
      
  | Arr vs -> 
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
            expNode', anyValueChanged || anyUpcast
      end
  | App (fn, args) -> annotate_app context expNode.exp_src fn args 
  | ArrayIndex (arr, indices) -> 
        failwith "annotation of array indexing not yet supported"
  in 
  expNode', changed || expNode.exp_types <> expNode'.exp_types  

and annotate_stmt context stmtNode = 
  let get_type id = 
    if ID.Map.mem id context.type_env 
    then ID.Map.find id context.type_env 
    else DynType.BottomT
  in  
  match stmtNode.stmt with 
  | Set(ids, rhs) ->  
      let rhs', rhsChanged = annotate_exp context rhs in 
      let oldTypes = List.map get_type ids in   
      let newTypes = rhs'.exp_types in 
      if List.length oldTypes <> List.length newTypes then 
        let errMsg = sprintf 
           "[annotate_stmt] malformed SET statement: \
             %d identifiers for %d rhs values \n"
           (List.length oldTypes)
           (List.length newTypes)
        in 
        failwith errMsg 
      else
      let commonTypes = List.map2 DynType.common_type oldTypes newTypes in 
      if List.exists ((=) DynType.AnyT) commonTypes then  
        failwith "error during type inference"
      
      else (
        IFDEF DEBUG THEN 
          assert (List.length ids = List.length newTypes); 
        ENDIF; 
        let tenv' = 
          List.fold_left2  
            (fun accEnv id t -> ID.Map.add id t accEnv)
            context.type_env 
            ids 
            newTypes
         in  
         let changed = rhsChanged || List.exists2 (<>) oldTypes newTypes in  
         let stmtNode' = { stmtNode with stmt = Set(ids, rhs') } in 
         stmtNode', tenv', changed 
      ) 
  | Ignore exp -> failwith "Ignore stmt not yet implemented"
  | SetIdx (id, indices, rhs) -> failwith "SetIdx stmt not yet implemented"
  | If (cond, tBlock, fBlock, gate) -> 
      let cond', _, condChanged = annotate_value context cond in
      let tBlock', tTyEnv, tChanged = annotate_block context tBlock in 
      let fBlock', fTyEnv, fChanged = annotate_block context fBlock in     
      
      let stmtNode' = {stmtNode with stmt = If(cond', tBlock', fBlock', gate) }
      in 
      let changed = condChanged || tChanged || fChanged in  
      (* apply type join operator to any variables two type environments*)
      (* have in common *) 
      let combine tenv2 id t = 
        if ID.Map.mem id tenv2 
        then DynType.common_type t (ID.Map.find id tenv2)
        else t 
      in 
      let (mergedTyEnv : DynType.t ID.Map.t) = 
        ID.Map.mapi (combine fTyEnv) (ID.Map.mapi (combine tTyEnv) fTyEnv) 
      in 
      (* add the output ids of the if-gate to the type env *)
      let aux env leftId rightId outputId =   
        let leftT = ID.Map.find leftId mergedTyEnv in 
        let rightT = ID.Map.find rightId mergedTyEnv in 
        ID.Map.add outputId (DynType.common_type leftT rightT) env
      in 
      let finalTyEnv = List.fold_left3 
        aux 
        mergedTyEnv 
        gate.SSA.true_ids  
        gate.SSA.false_ids
        gate.SSA.if_output_ids 
      in 
      stmtNode', finalTyEnv, changed 
          
and annotate_block context = function 
  | [] -> [], context.type_env, false 
  | stmtNode::rest -> 
        let stmtNode', tenv', currChanged = annotate_stmt context stmtNode in
        let context' = { context with type_env = tenv' } in 
        let rest', restTyEnv, restChanged  = annotate_block context' rest in
        (stmtNode' :: rest'), restTyEnv, currChanged || restChanged  

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
    InterpState.maybe_get_specialization interpState (GlobalFn untypedId) signature 
  with 
    | None ->
       let untypedFundef = InterpState.get_untyped_function interpState untypedId in 
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
  let aux (tyEnv, constEnv) id = function
    | Signature.Type t -> ID.Map.add id t tyEnv, constEnv 
    | Signature.Value v -> tyEnv, ID.Map.add id v constEnv
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
  let tyEnv, constEnv =
    List.fold_left2 aux 
      (ID.Map.empty, ID.Map.empty) 
      untypedFundef.input_ids 
      signature.Signature.inputs   
  in 
  let context = { 
    type_env = tyEnv; 
    const_env = constEnv; 
    interp_state = interpState 
  }
  in
  let annotatedBody, tenv', _ = annotate_block context untypedFundef.body in
  (* annotation returned a mapping of all identifiers to their types, *)
  (* now use this type info (along with annotatins on values) to insert all *)
  (* necessary coercions and convert untyped to typed Core *) 
  let context' = { context with type_env = tenv' } in
  (* returns both a typed body and an environment which includes bindings *)
  (* for any temporaries introduced from coercions *)  
  let typedBody, finalTyEnv = 
    InsertCoercions.rewrite_block context'.type_env annotatedBody in
  SSA.mk_fundef 
    ~body:typedBody 
    ~tenv:finalTyEnv
    ~input_ids:untypedFundef.input_ids
    ~output_ids:untypedFundef.output_ids
  
  
(****************************************************************************)
(*              SPECIALIZE FUNCTION VALUE                                   *)
(****************************************************************************)
and specialize_function_value interpState v signature : SSA.value_node = 
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
      let typedFundef = match v with
        (* specialize the untyped function -- assuming it is one *) 
      | GlobalFn untypedId  -> 
         specialize_function_id interpState untypedId signature
         
      (* first handle the simple case when it's a scalar op with scalar args *)
      | Prim (Prim.ScalarOp op) when 
          List.for_all DynType.is_scalar (Signature.input_types signature)  ->
        specialize_scalar_prim_for_scalar_args
          interpState 
          op 
          ?forceOutputType:(Option.map List.hd signature.Signature.outputs) 
          (Signature.input_types signature)
      | Prim (Prim.ScalarOp op) (* not scalar args *) -> 
        specialize_scalar_prim_for_vec_args
          interpState 
          op
          ?forceOutputType:(Option.map List.hd signature.Signature.outputs) 
          (Signature.input_types signature)
                   
      | Prim (Prim.ArrayOp op) when Prim.is_higher_order op ->
        (match signature.Signature.inputs with 
          | Signature.Value fnVal::restSig ->
            (* after the function value all the other arguments should *)
            (* be data to which we can assign types *) 
            let inputTypes = 
              List.map 
                (function 
                  | Signature.Type t -> t 
                  | _ -> failwith "unexpected value")
                restSig
            in  
            specialize_higher_order_array_prim 
              interpState
              op 
              [fnVal]
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
      | _ -> failwith "invalid value type for specialize_function_value"  
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
    (inputTypes :DynType.t list)
    : SSA.fundef  = 
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
    { (Signature.from_input_types eltTypes) with 
       Signature.outputs = forceOutputEltTypes }
  in 
  let nestedFn = specialize_function_value interpState f nestedSig in
  let outputTypes = 
    TypeInfer.infer_adverb Prim.Map (nestedFn.value_type::inputTypes) 
  in 
  let mapNode = { 
    value = Prim (Prim.ArrayOp Prim.Map);
    value_type = DynType.FnT(nestedFn.value_type::inputTypes, outputTypes); 
    value_src = None
  } 
  in
  SSA_Codegen.mk_lambda inputTypes outputTypes 
    (fun codegen inputs outputs -> 
      let appNode = { 
        exp = App(mapNode, nestedFn::inputs); 
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
          
  Printf.printf "[specialize_reduce] 1\n";
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
  Printf.printf "[specialize_reduce] 2\n";   
  let nestedFn = specialize_function_value interpState f nestedSig in
  Printf.printf "[specialize_reduce] 3\n";
  (* have to specialize twice in case output of reduction function doesn't *)
  (* match baseType *)
  let outputTypes =  DynType.fn_output_types nestedFn.value_type in 
  let fnNode = match outputTypes with 
    | [t] ->
       Printf.printf "[specialize] %s=?%s!\n"
         (DynType.to_str t)
         (DynType.to_str baseType)
         ;
      if t = baseType then (
       
          nestedFn
      ) 
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
  Printf.printf "[specialize_reduce] 3\n"; 
  let accType = List.hd outputTypes in 
  let reduceType = 
    DynType.FnT([fnNode.value_type; accType]@ vecTypes, [accType])
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
  let inputTypes = [inputType1; inputType2] in    
  let eltTypes = List.map DynType.peel_vec inputTypes in   
  let forceOutputEltTypes : DynType.t list option = 
    Option.map (List.map DynType.peel_vec) forceOutputTypes 
  in
  let nestedSig = 
    { (Signature.from_input_types eltTypes ) with 
       Signature.outputs = forceOutputEltTypes }
  in 
  let nestedFn = specialize_function_value interpState f nestedSig in
  let outputTypes = 
    TypeInfer.infer_adverb Prim.AllPairs (nestedFn.value_type :: inputTypes)
  in  
  let allPairsNode = { 
    value = Prim (Prim.ArrayOp Prim.AllPairs);
    value_type = DynType.FnT(nestedFn.value_type :: inputTypes, outputTypes); 
    value_src = None
  } 
  in
  
  SSA_Codegen.mk_lambda inputTypes outputTypes 
    (fun codegen inputs outputs -> 
      let appNode = { 
        exp = App(allPairsNode, nestedFn::inputs); 
        exp_types = outputTypes; 
        exp_src = None; 
      } 
      in 
      codegen#emit [mk_set (get_ids outputs) appNode]
    ) 
            
and specialize_higher_order_array_prim 
    ( interpState : InterpState.t ) 
    ( op  : Prim.array_op ) 
    ( fnVals : value list )  
    ?( forceOutputTypes : DynType.t list option ) 
    ( inputTypes : DynType.t list)
   : SSA.fundef  =  
  match op, fnVals, inputTypes with 
  | Prim.Map, [f], _ -> 
    specialize_map interpState f ?forceOutputTypes inputTypes
  (* for reduction, split the type of the initial value from the vectors *)
  (* being reduced *)  
  | Prim.Reduce,  [f], (initType::vecTypes) -> 
    specialize_reduce interpState f ?forceOutputTypes initType vecTypes
  | Prim.AllPairs, [f], [t1; t2] -> 
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
 (*match op with 
    | Prim.Where ->
        
        in  
        
    | Prim.Index, _ -> failwith "index not yet implemented"
    | other, types -> 
        failwith $ Printf.sprintf 
        "[Specialize] specializtion of %s not implemented for input of type %s"
        (Prim.array_op_to_str other)
        (DynType.type_list_to_str types) 
*)
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
    | _ ->  failwith "Q's not welcome here"
            