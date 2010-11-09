open Base 

type program (*= {
  untyped_functions : FnTable.t;
  
  typed_functions : FnTable.t;
  
  (* functions are either ID or Prim which gets specialized on either *)
  (* types or values (see Signature.ml)   *)    
  specializations : (SSA.value * Signature.t, FnId.t) Hashtbl.t;
  
  
  name_to_untyped_id : (string, SSA.FnId.t) Hashtbl.t;
  
  untyped_id_to_name : (SSA.FnId.t, string) Hashtbl.t;   
} 
*)
val add_specialization : 
      program -> SSA.value -> Signature.t -> SSA.fundef -> unit 
      
val maybe_get_specialization : 
      program -> SSA.value -> Signature.t -> SSA.FnId.t option 
      
val get_untyped_function : program -> SSA.FnId.t -> SSA.fundef 

val get_typed_function : program -> SSA.FnId.t -> SSA.fundef 

val optimize_typed_functions : program -> unit

val optimize_untyped_functions : program -> unit     

val create_untyped : 
      SSA.FnId.t String.Map.t -> SSA.fundef SSA.FnId.Map.t -> program 
      
val get_untyped_name : program -> SSA.FnId.t -> string 
val get_untyped_id : program -> string -> SSA.FnId.t 

val get_typed_function_table : program -> FnTable.t 
val get_untyped_function_table : program -> FnTable.t 
    