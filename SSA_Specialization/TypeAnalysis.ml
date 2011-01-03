(* pp: -parser o pa_macro.cmo *)
open Printf 
open Base
open SSA 
open SSA_Analysis

module type TYPE_ANALYSIS_PARAMS = sig 
  val closures : (ID.t, value) Hashtbl.t
  val closure_args : (ID.t, value_node list) Hashtbl.t
  val closure_arity : (ID.t, int) Hashtbl.t
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
  let flow_merge outEnv outId leftEnv leftId rightEnv rightId = None

  let flow_split env = env, env 
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
        if Hashtbl.mem P.closures id then
          let fnVal' = Hashtbl.find P.closures id in 
          let closureArgNodes = Hashtbl.find P.closure_args id in 
          let closureArgTypes = List.map (value tenv) closureArgNodes in
          infer_app tenv fnVal' (closureArgTypes @ argTypes)  
        else assert false 
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
    | _ -> failwith "not implemented"     


  let exp tenv expNode info = match expNode.exp, info with
    | App({value=Prim (Prim.ArrayOp arrayOp)}, args), AppInfo(_, argTypes)
        when Prim.is_higher_order arrayOp -> 
          infer_higher_order tenv arrayOp args argTypes
    | App (fn, _), AppInfo(fnT, argTypes) when DynType.is_vec fnT -> 
        [TypeInfer.infer_simple_array_op Prim.Index (fnT::argTypes)]   
    | App(fn, _), AppInfo(fnT, argTypes)-> infer_app tenv fn.value argTypes  
    | _, ArrayInfo eltTypes ->
      let commonT = DynType.fold_type_list eltTypes in 
      assert (commonT <> DynType.AnyT); 
      [DynType.VecT commonT]
    | _, ValuesInfo types -> types  
    | _ -> failwith "not implemented"
  
  let stmt tenv stmtNode stmtInfo = match stmtNode.stmt, stmtInfo with 
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
          if changedT then add_type tenv id newT else tenv 
        in 
        tenv', (changed || changedT)
      in 
      let tenv', changed = 
        List.fold_left2 process_types (tenv, false) ids rhsTypes
      in  
      if changed then Some tenv' else None 
    | _ -> failwith "not implemented"
end
   
let type_analysis infer_output_types closureEnv fundef signature = 
  let module Params : TYPE_ANALYSIS_PARAMS = struct 
    let closures = closureEnv.CollectPartialApps.closures
    let closure_args = closureEnv.CollectPartialApps.closure_args 
    let closure_arity = closureEnv.CollectPartialApps.closure_arity 
    let infer_output_types = infer_output_types
    let signature = signature  
  end    
  in
  let module TypeEval = MkEvaluator(MkAnalysis(Params)) in 
  TypeEval.eval_fundef fundef 
    