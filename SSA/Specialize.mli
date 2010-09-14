type type_env = (ID.t, DynType.t) PMap.t

type const_env = (ID.t, SSA.value) PMap.t
   
type context = {
  type_env : type_env;
  const_env : const_env;
  program : Program.program 
}  

type change_indicator = bool 

(* SPECIALIZE creates an initial type environment, and then calls *)
(* ANNOTATE. The annotator performs iterative local type inference/annotation *)
(* on the function body until types stop changing. The annotator may call back *)
(* into SPECIALIZE when it encounters a function call and needs to know its *)
(* return type. The specializer lastly calls REWRITE which uses type *)
(* annotations to insert any necessary coercions *) 

val specialize_function_value 
    : Program.program-> SSA.value -> Signature.t -> SSA.value_node 

val specialize_fundef 
    : Program.program -> SSA.fundef -> Signature.t -> SSA.fundef 
  
val specialize_function_id 
    : Program.program -> ID.t -> Signature.t -> SSA.fundef


val annotate_block 
    : context -> SSA.block -> 
        (SSA.block * type_env * change_indicator)
         
val annotate_stmt 
    : context -> SSA.stmt_node -> (SSA.stmt_node * type_env * change_indicator)
         
val annotate_exp 
    : context -> SSA.exp_node -> (SSA.exp_node * change_indicator)
    
val annotate_value 
    : context -> SSA.value_node -> (SSA.value_node * change_indicator)  

    
    
