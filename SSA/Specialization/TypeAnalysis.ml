(* pp: -parser o pa_macro.cmo *)
open Printf 
open Base
open SSA 
open SSA_Analysis

module type TYPE_ANALYSIS_PARAMS = sig 
  val closure_val : ID.t -> value 
  val closure_args : ID.t -> value_node list
  val output_arity : value -> int 
  val infer_output_types : value -> Signature.t -> DynType.t list   
  val signature : Signature.t 
end
 
let get_type tenv id = Hashtbl.find_default tenv id DynType.BottomT  
let add_type tenv id t = Hashtbl.add tenv id t; tenv 


(* TODO: make this complete for all SSA statements *) 
let rec is_scalar_stmt = function
  | SSA.Set(_, {exp=SSA.App({value=SSA.Prim (Prim.ScalarOp _)}, _)})
  | SSA.Set(_, {exp=Values _}) -> true 
  | SSA.If(_, tCode, fCode, _) -> 
      all_scalar_stmts tCode && all_scalar_stmts fCode
  | _ -> false   
and is_scalar_stmt_node stmtNode = is_scalar_stmt stmtNode.stmt 
and all_scalar_stmts stmts = Block.for_all is_scalar_stmt_node stmts    

module MkAnalysis (P : TYPE_ANALYSIS_PARAMS) = struct
  let iterative = true
  let dir = Forward 
  
  type env = (ID.t, DynType.t) Hashtbl.t  
  type exp_info = DynType.t list
  type value_info = DynType.t 
  
  let init fundef =
    let inTypes = Signature.input_types P.signature in
    let tenv = Hashtbl.create 127 in
    List.iter2 (fun id t -> Hashtbl.add tenv id t) fundef.input_ids inTypes;    
    (if Signature.has_output_types P.signature then 
      let outTypes = Signature.output_types P.signature in
      List.iter2 (fun id t -> Hashtbl.add tenv id t) fundef.output_ids outTypes   
    );  
    tenv 
   
   
  let infer_value_type tenv = function 
    | Var id -> get_type tenv id
    | Num n -> PQNum.type_of_num n
    | Str _ -> DynType.StrT
    | Sym _ -> DynType.SymT
    | Unit -> DynType.UnitT
    | _ -> DynType.AnyT   
  
  let value tenv vNode = infer_value_type tenv vNode.value 
  
  let phi_set tenv id t = 
    try (
      let oldT = Hashtbl.find tenv id in 
      if oldT = t then None 
      else 
        let t' = DynType.common_type oldT t in
        (Hashtbl.replace tenv id t'; Some tenv)
    )
    with _ -> Hashtbl.replace tenv id t; Some tenv
  
  let phi_merge tenv id tLeft tRight = 
    phi_set tenv id (DynType.common_type tLeft tRight)  
       
           
  let rec infer_app tenv fnVal (argTypes:DynType.t list) = match fnVal with
    | Var id ->
        (* if the identifier would evaluate to a function value...*) 
        let fnVal' = P.closure_val id in
        let closureArgNodes = P.closure_args id in  
        let closureArgTypes = List.map (value tenv) closureArgNodes in
        infer_app tenv fnVal' (closureArgTypes @ argTypes)   
    | Prim (Prim.ArrayOp arrayOp) ->
        [TypeInfer.infer_simple_array_op arrayOp argTypes]
    | Prim (Prim.ScalarOp scalarOp) -> 
        [TypeInfer.infer_scalar_op scalarOp argTypes] 
    | Prim (Prim.Q_Op qOp) -> [TypeInfer.infer_q_op qOp argTypes]
    | GlobalFn _ -> 
        let signature = Signature.from_input_types argTypes in
        P.infer_output_types fnVal signature 
             
    | _ -> 
       failwith $ 
          Printf.sprintf 
            "Inference for function application where fn = %s not implemented"
            (SSA.value_to_str fnVal) 

        
  let infer_higher_order tenv arrayOp args argTypes =
    IFDEF DEBUG THEN 
      Printf.printf "[TypeAnalysis] %s(%s) : %s\n"
        (Prim.adverb_to_str arrayOp)
        (SSA.value_nodes_to_str args)
        (DynType.type_list_to_str argTypes);
    ENDIF; 
    match arrayOp, args, argTypes with 
    | Prim.Map, {value=fnVal}::_, _::dataTypes ->
        if List.for_all DynType.is_scalar dataTypes then 
          failwith "expected at least one argument to map to be a vector"
        ; 
        (* we're assuming Map works only along the outermost axis of an array *) 
        let eltTypes = List.map DynType.peel_vec dataTypes in 
        let eltResultTypes = infer_app tenv fnVal eltTypes in 
        List.map (fun t -> DynType.VecT t) eltResultTypes     
    | Prim.Reduce, {value=fnVal}::_, _::argTypes ->
        let arity = P.output_arity fnVal in 
        let initTypes, vecTypes = List.split_nth arity argTypes in
        let eltTypes = List.map DynType.peel_vec vecTypes in 
        let accTypes = infer_app tenv fnVal (initTypes @ eltTypes) in
        let accTypes' = infer_app tenv fnVal (accTypes @ eltTypes) in 
        if accTypes <> accTypes' then 
          failwith "unable to infer accumulator type"
        ; 
        accTypes 
    | Prim.AllPairs, {value=fnVal}::_, _::argTypes ->  
        let eltTypes = List.map DynType.peel_vec argTypes in 
        let outTypes = infer_app tenv fnVal  eltTypes in
        List.map (fun t -> DynType.VecT (DynType.VecT t)) outTypes 
        
    | other, _, _ -> failwith (Prim.adverb_to_str other ^ " not impl")     

  let exp tenv expNode helpers = match expNode.exp with 
    | App({value=SSA.Prim (Prim.Adverb arrayOp)}, args) ->
        infer_higher_order tenv arrayOp args (helpers.eval_values tenv args)
    | App(fn, args) ->
        let fnT = value tenv fn in 
        let argTypes = helpers.eval_values tenv args in 
        if DynType.is_vec fnT then   
          [TypeInfer.infer_simple_array_op Prim.Index (fnT::argTypes)]
        else infer_app tenv fn.value argTypes   
    | Arr elts -> 
        let commonT = DynType.fold_type_list (helpers.eval_values tenv elts) in
        assert (commonT <> DynType.AnyT); 
        [DynType.VecT commonT]
  
    | Values vs -> helpers.eval_values tenv vs  
    | _ -> failwith $ Printf.sprintf 
            "Type analysis not implemented for expression: %s"
            (SSA.exp_to_str expNode)

  let stmt tenv stmtNode helpers = match stmtNode.stmt with 
    | Set(ids, rhs) ->
      let types = exp tenv rhs helpers in   
      IFDEF DEBUG THEN
        if List.length ids <> List.length types then 
          failwith $ sprintf 
            "malformed SET statement: %d ids for %d rhs values \n"
            (List.length ids)
            (List.length types)
      ENDIF; 
      let rec process_types (tenv, changed) id rhsT =  
        IFDEF DEBUG THEN 
          if rhsT = DynType.AnyT then failwith "error during type inference"
        ENDIF; 
        let oldT = get_type tenv id in
        let newT = DynType.common_type oldT rhsT in 
        let changedT = oldT <> newT in
        let tenv' = if changedT then add_type tenv id newT else tenv in 
        tenv', (changed || changedT)
      in 
      let tenv', changed = 
        List.fold_left2 process_types (tenv, false) ids types
      in  
      if changed then Some tenv' else None 
   | _ -> helpers.eval_stmt tenv stmtNode 
    (* 
    (*| If (cond, tBlock, fBlock, merge) ->*)  
    | _ -> failwith $ Printf.sprintf 
            "Type analysis not implemented for statement: %s"
            (SSA.stmt_node_to_str stmtNode)
    *)
end

let type_analysis 
      ~(specializer:SSA.value-> Signature.t -> SSA.fundef) 
      ~(output_arity: SSA.value -> int)
      ~(closureEnv:CollectPartialApps.closure_env) 
      ~(fundef:SSA.fundef) 
      ~(signature:Signature.t) =
  let module Params : TYPE_ANALYSIS_PARAMS = struct 
    let closure_val = 
      (fun id -> Hashtbl.find closureEnv.CollectPartialApps.closures id)
    let closure_args = 
      (fun id -> Hashtbl.find closureEnv.CollectPartialApps.closure_args id) 
    let output_arity = output_arity  
    let infer_output_types = 
      (fun fnVal fnSig -> 
        let fundef = specializer fnVal fnSig in fundef.fn_output_types)
    let signature = signature  
  end    
  in
  let module TypeEval = MkEvaluator(MkAnalysis(Params)) in 
  TypeEval.eval_fundef fundef 
    