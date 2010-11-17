type type_env = DynType.t ID.Map.t
type const_env = SSA.value ID.Map.t 
type untyped_closure_env = (SSA.value * Signature.sig_elt list) ID.Map.t 
type typed_closure_env = (FnId.t * DynType.t list) ID.Map.t

(* map untyped closure ids to their specializations  *) 
type typed_closure_mapping = ID.Set.t ID.Map.t   

type context = {
  type_env : type_env; 
  const_env : const_env; 
  typed_closures : typed_closure_env; 
  untyped_closures : untyped_closure_env;
  (* map untyped closure id to typed variant *) 
  typed_closure_mapping  : typed_closure_mapping;
  interp_state : InterpState.t 
}  


type change_indicator = bool 

(* SPECIALIZE creates an initial type environment, and then calls *)
(* ANNOTATE. The annotator performs iterative local type inference/annotation *)
(* on the function body until types stop changing. The annotator may call back *)
(* into SPECIALIZE when it encounters a function call and needs to know its *)
(* return type. The specializer lastly calls REWRITE which uses type *)
(* annotations to insert any necessary coercions *) 

val specialize_function_value 
    : InterpState.t-> SSA.value -> Signature.t -> SSA.value_node 

val specialize_fundef 
    : InterpState.t -> SSA.fundef -> Signature.t -> SSA.fundef 
  
val specialize_function_id 
    : InterpState.t -> ID.t -> Signature.t -> SSA.fundef

val annotate_block 
    : context -> SSA.block -> 
      (SSA.block * context * change_indicator)
         
val annotate_stmt 
    : context -> SSA.stmt_node -> 
      (SSA.stmt_node * context * change_indicator)

                 
val annotate_exp 
    : context -> SSA.exp_node -> (SSA.exp_node * context * change_indicator)
    
val annotate_value 
    : context -> SSA.value_node -> 
        (SSA.value_node * DynType.t * change_indicator)  

val annotate_values 
    : context -> SSA.value_node list -> 
         (SSA.value_node list * DynType.t list * change_indicator)
    
    
