(* pp: -parser o pa_macro.cmo *)
open Base
open SSA 

module type TYPE_ANALYSIS_PARAMS = sig 
  val interpState : InterpState.t
  val specialize : fundef -> Signature.t -> fundef    
end

module TypeAnalysis (T : TYPE_ANALYSIS_PARAMS) = struct
  type env = context 
  type exp_info = DynType.t list
  type value_info = DynType.t 
  
  let init _ = { 
    type_env = ID.Map.empty;
    untyped_closures = Hashtbl.create 127;
    untyped_closure_args = Hashtbl.create 127;
    typed_closure_mapping = Hashtbl.create 127;
    interp_state = T.interpState 
  }
  
  let set context ids rhsNode rhsTypes =
    IFDEF DEBUG THEN
      if List.length ids <> List.length rhsTypes then 
        failwith $ sprintf 
          "[annotate_stmt] malformed SET statement: %d ids for %d rhs values \n"
          (List.length ids)
          (List.length rhsTypes)
    ENDIF; 
    let rec type_folder (tenv, changed) id rhsT =  
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
      List.fold_left2 type_folder (context.type_env, false) ids rhsTypes
    in  
    if changed then Some { context with type_env = tenv' } else None 
         
end
and TypeEval : SSA_Analysis.EVALUATOR = SSA_Analysis.MakeEvaluator(TypeAnalysis) 
and Specializer : sig 
  val specialize : InterpState.t -> fundef -> Signature.t -> fundef
end = struct 
  let specialize interpState fundef signature =
    TypeAnalysis.set_interp_state interpState; 
    TypeEval.eval_fundef fundef        
end
