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
and all_scalar_stmts stmts = SSA.block_for_all is_scalar_stmt_node stmts    

module MkAnalysis (P : TYPE_ANALYSIS_PARAMS) = struct
  let iterative = true
  let clone_env env = env
  let flow_merge outEnv outId leftEnv leftId rightEnv rightId = None
 
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
    | GlobalFn _ -> 
        let signature = Signature.from_input_types argTypes in
        P.infer_output_types fnVal signature 
             
    | _ -> assert false

        
  let infer_higher_order tenv arrayOp args argTypes =
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
      failwith "no all-pairs impl yet"
    | other, _, _ -> failwith (Prim.adverb_to_str other ^ " not impl")     


  let exp_app tenv expNode ~fn ~args ~fnInfo ~argInfo = 
    match fn.value with 
    | Prim (Prim.Adverb arrayOp) -> 
      infer_higher_order tenv arrayOp args argInfo
    | _  when  DynType.is_vec fnInfo ->   
      [TypeInfer.infer_simple_array_op Prim.Index (fnInfo::argInfo)]   
    | fnVal ->  infer_app tenv fnVal argInfo  
  
  let exp_arr tenv expNode ~elts ~info = 
    let commonT = DynType.fold_type_list info in 
    assert (commonT <> DynType.AnyT); 
    [DynType.VecT commonT]
  
  let exp_values tenv expNode ~vs ~info = info 

  let exp_primapp _ _ ~prim ~args ~argInfo = 
    failwith "unexpected typed prim app"

  let exp_call _ _ ~fnId ~args ~info = 
    failwith "unexpected typed function call"
  let exp_scan
        _ _ ~initClosure ~scanClosure ~args ~initInfo ~scanInfo ~argInfo =
        failwith "unexpected typed Scan"
  let exp_reduce 
        _ _ ~initClosure ~reduceClosure ~args ~initInfo ~reduceInfo ~argInfo = 
        failwith "unexpected typed Reduce"   
  let exp_map _ _ ~closure ~args ~closureInfo ~argInfo = 
        failwith "unexpected typed Map"
  
  let stmt_set tenv stmtNode ~ids ~rhs ~rhsInfo = 
    IFDEF DEBUG THEN
      if List.length ids <> List.length rhsInfo then 
        failwith $ sprintf 
          "malformed SET statement: %d ids for %d rhs values \n"
          (List.length ids)
          (List.length rhsInfo)
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
      List.fold_left2 process_types (tenv, false) ids rhsInfo
    in  
    if changed then Some tenv' else None 
 
    let stmt_if env stmtNode ~cond ~tBlock ~fBlock ~gate ~condInfo ~tEnv ~fEnv =
      failwith "IF not implemented"
 
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
    