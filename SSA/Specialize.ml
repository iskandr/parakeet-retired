open Base
open SSA
open SSA_Codegen 
open DynType
open Printf 


type type_env = (ID.t, DynType.t) PMap.t
type const_env = (ID.t, SSA.value) PMap.t

type context = {
  type_env : type_env; 
  const_env : const_env; 
  program : Program.program 
}  

type change_indicator = bool    
  
let sig_from_type = function 
  | FnT(inTypes, outTypes) -> Signature.from_types inTypes outTypes 
  | _ -> failwith "expected function type"

let annotate_value context valNode = 
  let t = match valNode.value with 
  | Var id -> PMap.find id context.type_env  
  | Num n -> PQNum.type_of_num n 
  | Str _ -> DynType.StrT 
  | Sym _ -> DynType.SymT  
  | Unit -> DynType.UnitT
  | Prim _ -> DynType.AnyFnT 
    (*failwith "cannot annotate primitive without its arguments"*)
  | Lam _ -> DynType.AnyFnT 
    (*failwith "cannot annotate a function without its arguments"*) 
 in 
    (* if the new type is different from the old, *)
    (* then indicate that a change has happened *)    
    let changed = valNode.value_type <> t in 
    let valNode' = {valNode with value_type = t} in  
    valNode', changed  

let rec annotate_values context = function 
  | [] -> [], false  
  | v::vs ->
      let v', currChanged = annotate_value context v in 
      let vs', restChanged = annotate_values context vs in 
      v'::vs', currChanged || restChanged 

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
   
  let expectedTypes = 
    TypeInfer.required_op_arg_types (Prim.ScalarOp op) inputTypes
  in 
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

        
let rec annotate_exp context expNode =
  (* if the overall types returned by the expression change then *)
  (* this taken into account at the bottom of the function *)  
  let expNode', changed = match expNode.exp with 
  | Values vNodes ->
      let vNodes', anyChanged = annotate_values context vNodes in
      let expNode' = { expNode with 
        exp= Values vNodes'; 
        exp_types = List.map (fun v -> v.value_type) vNodes' 
      } 
      in 
      expNode', anyChanged  
  | Cast(t, vNode) -> 
      let vNode', changed = annotate_value context vNode in 
      if vNode'.value_type = t then 
        let expNode' = { expNode with exp = Values [vNode']; exp_types = [t] } 
        in 
        expNode', changed 
      else 
        let expNode' = { expNode with exp= Cast(t, vNode'); exp_types = [t] } 
        in 
        expNode', changed
         
  | Arr vs -> 
      let vs', anyValueChanged = annotate_values context vs in 
      let types = List.map (fun valNode -> valNode.value_type) vs' in 
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
  
  | App (fn, args) -> 
     (*
       THIS IS THE HEART OF THE TYPE INFERENCE / SPECIALIZATION ALGORITHM:
       ANNOTATION AND SPECIALIZATION ARE BOUND TOGETHER THROUGH POLYVARIANCE. 
       ----------------------------------------------------------------------
       Polyvariance means that at every function call site we specialize 
       that function for that particular argument signature. In the case 
       of higher order functions the function values are included in the 
       signature. 
     *) 
       begin match fn.value with
       (* for now assume that all higher order primitives take a single *)
       (* function argument which passed first *)  
       | Prim (Prim.ArrayOp p) when Prim.is_higher_order p -> 
          assert (List.length args > 1); 
          let fnArg = List.hd args in 
          let dataArgs = List.tl args in 
          let dataArgs', argsChanged = annotate_values context dataArgs in
          let types = List.map (fun v -> v.value_type) dataArgs' in
          (* for now just pass through the literal value but should eventually*)
          (* have a context available with an environment of literal value *)
          (* arguments and check whether the fnArg is in that environment. *)  
          let signature = { 
            Signature.inputs = (Signature.Value fnArg.value)::
              (List.map (fun t -> Signature.Type t) types);
            Signature.outputs = None
          } 
          in 
          let specializedFn = 
             specialize_function_value context.program fn.value signature 
           in
           let outTypes = DynType.fn_output_types specializedFn.value_type in 
           let expNode' = { expNode with 
             exp_types = outTypes; 
             exp = App(specializedFn, dataArgs') 
           }
           in expNode', argsChanged
      
       (* for now assume all named functions are top-level and that none*)
       (* of the arguments are themselves functions. This means that for now *)
       (* the only higher order functions are built in primitives. *)  
       | Var _ 
       | Prim _ ->
           let args', argsChanged = annotate_values context args in 
           let types = List.map (fun valNode -> valNode.value_type) args' in
           let signature = Signature.from_input_types types in 
           let specializedFn = 
             specialize_function_value context.program fn.value signature 
           in
           let outTypes = DynType.fn_output_types specializedFn.value_type in 
           let expNode' = { expNode with 
             exp_types = outTypes; 
             exp = App(specializedFn, args') 
           }
           in expNode', argsChanged               
       | Lam _ -> 
            failwith "anonymous functions should have been named by now"
       | _ -> failwith "invalid object where function was expected"
       end 
  | ArrayIndex (arr, indices) -> 
        failwith "annotation of array indexing not yet supported"
  in 
  expNode', changed || expNode.exp_types <> expNode'.exp_types  

and annotate_stmt context stmtNode = 
  let get_type id = 
    if PMap.mem id context.type_env 
    then PMap.find id context.type_env 
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
      else
      let tenv' = 
        List.fold_left2  
            (fun accEnv id t -> PMap.add id t accEnv)
            context.type_env 
            ids 
            newTypes
       in  
       let changed = rhsChanged || List.exists2 (<>) oldTypes newTypes in  
       let stmtNode' = { stmtNode with stmt = Set(ids, rhs') } in 
       stmtNode', tenv', changed 
       
  | Ignore exp -> failwith "Ignore stmt not yet implemented"
  | SetIdx (id, indices, rhs) -> failwith "SetIdx stmt not yet implemented"
  | If (cond, tBlock, fBlock, gate) -> 
      let cond', condChanged = annotate_value context cond in
      let tBlock', tTyEnv, tChanged = annotate_block context tBlock in 
      let fBlock', fTyEnv, fChanged = annotate_block context fBlock in     
      
      let stmtNode' = {stmtNode with stmt = If(cond', tBlock', fBlock', gate) }
      in 
      let changed = condChanged || tChanged || fChanged in  
      (* apply type join operator to any variables two type environments*)
      (* have in common *) 
      let combine tenv2 id t = 
        if PMap.mem id tenv2 
        then DynType.common_type t (PMap.find id tenv2)
        else t 
      in 
      let (mergedTyEnv : (ID.t, DynType.t) PMap.t) = 
        PMap.mapi (combine fTyEnv) (PMap.mapi (combine tTyEnv) fTyEnv) 
      in 
      (* add the output ids of the if-gate to the type env *)
      let aux env leftId rightId outputId =   
        let leftT = PMap.find leftId mergedTyEnv in 
        let rightT = PMap.find rightId mergedTyEnv in 
        PMap.add outputId (DynType.common_type leftT rightT) env
      in 
      let finalTyEnv = List.fold_left3 
        aux 
        mergedTyEnv 
        gate.SSA_Gates.true_ids  
        gate.SSA_Gates.false_ids
        gate.SSA_Gates.if_output_ids 
      in 
      stmtNode', finalTyEnv, changed 
          
and annotate_block context = function 
  | [] -> [], context.type_env, false 
  | stmtNode::rest -> 
        let stmtNode', tenv', currChanged = annotate_stmt context stmtNode in
        let context' = { context with type_env = tenv' } in 
        let rest', restTyEnv, restChanged  = annotate_block context' rest in
        (stmtNode' :: rest'), restTyEnv, currChanged || restChanged  
        
and specialize_function_id program untypedId signature =
  match Program.maybe_get_specialization program (Var untypedId) signature with 
    | None ->
       let untypedFundef = Program.get_untyped_function program untypedId in 
       let typedFundef = specialize_fundef program untypedFundef signature in
       let _ = 
         Program.add_specialization 
           program (Var untypedId) signature typedFundef
       in
       typedFundef  
    | Some typedId -> Program.get_typed_function program typedId 
          
and specialize_fundef program untypedFundef signature = 
  let aux (tyEnv, constEnv) id = function
    | Signature.Type t -> PMap.add id t tyEnv, constEnv 
    | Signature.Value v -> tyEnv, PMap.add id v constEnv
  in  
  let tyEnv, constEnv =
    List.fold_left2 aux 
      (PMap.empty, PMap.empty) 
      untypedFundef.input_ids 
      signature.Signature.inputs   
  in 
  let context = { type_env = tyEnv; const_env = constEnv; program = program } in 
  let annotatedBody, tenv', _ = annotate_block context untypedFundef.body in
  (* annotation returned a mapping of all identifiers to their types, *)
  (* now use this type info (along with annotatins on values) to insert all *)
  (* necessary coercions and convert untyped to typed Core *) 
  let context' = { context with type_env = tenv' } in
  (* returns both a typed body and an environment which includes bindings *)
  (* for any temporaries introduced from coercions *)  
  let typedBody, finalTyEnv = 
    InsertCoercions.rewrite_block context'.type_env annotatedBody in
  let inTypes = 
    List.map (fun id -> PMap.find id finalTyEnv) untypedFundef.input_ids in 
  let outTypes =
    List.map (fun id -> PMap.find id finalTyEnv) untypedFundef.output_ids in 
  { body = typedBody; 
    tenv = finalTyEnv;
    input_ids = untypedFundef.input_ids; 
    fun_type = FnT (inTypes, outTypes);
    output_ids = untypedFundef.output_ids; 
  }
(****************************************************************************)
(*              SPECIALIZE FUNCTION VALUE                                   *)
(****************************************************************************)
and specialize_function_value program v signature = 
  match Program.maybe_get_specialization program v signature with 
    | Some fnId -> 
        let fundef =  Program.get_typed_function program fnId in 
        { value = Var fnId; value_type = fundef.fun_type; value_src = None }
           
    | None -> 
      let typedFundef = match v with
        (* specialize the untyped function -- assuming it is one *) 
      | Var untypedId  -> 
        let untypedFundef = Program.get_untyped_function program untypedId in 
        specialize_fundef program untypedFundef signature
       
      (* first handle the simple case when it's a scalar op with scalar args *)
      | Prim (Prim.ScalarOp op) when 
          List.for_all DynType.is_scalar (Signature.input_types signature)  ->
        specialize_scalar_prim_for_scalar_args
          program 
          op 
          ?forceOutputType:(Option.map List.hd signature.Signature.outputs) 
          (Signature.input_types signature)
      | Prim (Prim.ScalarOp op) (* not scalar args *) -> 
        specialize_scalar_prim_for_vec_args
          program 
          op
          ?forceOutputType:(Option.map List.hd signature.Signature.outputs) 
          (Signature.input_types signature)
                   
      | Prim (Prim.ArrayOp op) ->
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
            specialize_array_prim 
              program
              op 
              [fnVal]
              ?forceOutputTypes:signature.Signature.outputs
              inputTypes

          | _ -> failwith "expected function value"
        )
      | _ -> failwith "invalid value type for specialize_function_value"  
     in 
     let typedId = 
       Program.add_specialization program v signature  typedFundef 
     in
     { value = Var typedId; 
       value_type = typedFundef.fun_type; 
       value_src = None 
     }      

  (* if we're applying a scalar operator to at least one vector, transform *)
  (* the whole operation into a map *) 
and specialize_scalar_prim_for_vec_args program op ?forceOutputType inputTypes 
    : SSA.fundef =  
    let eltTypes = List.map DynType.elt_type inputTypes in
    let nestedSig = {
      Signature.inputs = List.map (fun t -> Signature.Type t) eltTypes; 
      outputs = None
    } 
    in 
    let nestedFn = 
      specialize_function_value program (Prim (Prim.ScalarOp op)) nestedSig  in
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
    program 
    (f : value) 
    ?(forceOutputTypes : DynType.t list option) 
    (inputTypes :DynType.t list)
    : SSA.fundef  = 
  let eltTypes =  List.map DynType.elt_type inputTypes in 
  (* if the outputs need to be coerced, pass on that information to the *)
  (* nested function so that coercions get inserted at the scalar level.*)
  (* For example, if we have "map {x*x} 1 2 3" : float vec, it's preferable*)
  (* to specialize into " map {[x] y = cast(x,float); y*y} 1 2 3" *)
  (* rather than:  "cast(map {x*x} 1 2 3, float vec)" *)
  let forceOutputEltTypes : DynType.t list option = 
    Option.map (List.map DynType.elt_type) forceOutputTypes 
  in
  let nestedSig = 
    { (Signature.from_input_types eltTypes) with 
       Signature.outputs = forceOutputEltTypes }
  in 
  let nestedFn = specialize_function_value program f nestedSig in
  let nestedOutputTypes = DynType.fn_output_types nestedFn.value_type in 
  let outputTypes = List.map (fun t -> DynType.VecT t) nestedOutputTypes in
  let mapNode = { 
    value = Prim (Prim.ArrayOp Prim.Map);
    value_type = DynType.FnT(eltTypes, nestedOutputTypes); 
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
and specialize_reduce program f ?forceOutputTypes baseType vecTypes 
        : SSA.fundef =
  let forceOutputEltTypes = 
    Option.map (List.map DynType.elt_type ) forceOutputTypes 
  in
  
  let nestedVecTypes = List.map DynType.elt_type vecTypes in  
  let nestedSig = {
      Signature.inputs = 
        List.map (fun t -> Signature.Type t) (baseType::nestedVecTypes); 
      outputs = forceOutputEltTypes; 
  } 
  in   
  let nestedFn = specialize_function_value program f nestedSig in
  (* have to specialize twice in case output of reduction function doesn't *)
  (* match baseType *)
  let outputTypes =  DynType.fn_output_types nestedFn.value_type in 
  let nestedFn' = match outputTypes with 
    | [t] when t = baseType -> nestedFn
    | [t] -> 
        let nestedSig' = { nestedSig with Signature.outputs = Some [t] } in 
        specialize_function_value program f nestedSig' 
    | [] -> failwith "reduction function with 0 outputs not supported"
    | _ -> failwith "reduction function with multiple outputs not supported"
  in 
  let baseType' = List.hd outputTypes in 
  let reduceType = 
    DynType.FnT([nestedFn'.value_type; baseType']@ vecTypes, [baseType'])
  in 
  let reduceNode = 
    { value=Prim (Prim.ArrayOp Prim.Reduce); 
      value_type = reduceType;  
      value_src = None 
    }
  in 
  let fnNode = nestedFn' in  
  mk_lambda (baseType::vecTypes) outputTypes
   (fun codegen inputs outputs -> 
      let baseVal = List.hd inputs in 
      let baseVal' =
         codegen#cvt ~from_type:baseType ~to_type:baseType' baseVal in
      let appNode = 
      { exp = App(reduceNode, fnNode :: baseVal' :: (List.tl inputs));  
        exp_types = [baseType']; 
        exp_src = None;
      }
      in   
      codegen#emit [mk_set (get_ids outputs) appNode] 
   )      
      
and specialize_array_prim 
    ( program : Program.program ) 
    ( op  : Prim.array_op ) 
    ( fnVals : value list )  
    ?( forceOutputTypes : DynType.t list option ) 
    ( inputTypes : DynType.t list)
   : SSA.fundef  =  
 
  match op, fnVals, inputTypes with 
  | Prim.Map, [f], _ -> 
    specialize_map program f ?forceOutputTypes inputTypes
  (* for reduction, split the type of the initial value from the vectors *)
  (* being reduced *)  
  | Prim.Reduce,  [f], (initType::vecTypes) -> 
    specialize_reduce program f ?forceOutputTypes initType vecTypes 
  | _ -> failwith "[specialize] array operator not yet implemented" 
