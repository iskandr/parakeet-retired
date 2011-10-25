open SSA 

(* TODO: get rid of this nonsense *)
val extract_nested_map_fn_id : fn -> FnId.t option 
    
val mk_fn : 
      ?tenv:tenv -> input_ids:ID.t list -> output_ids:ID.t list -> 
        body:block -> fn 
(***
     helpers for statements 
 ***) 

val mk_stmt : ?src:SrcInfo.t -> ?id:StmtId.t -> stmt -> stmt_node 
val mk_set : ?src:SrcInfo.t -> ID.t list -> exp_node -> stmt_node 
      
(* get the id of a variable value node *) 
val get_id : value_node -> ID.t 

(* get the ids from a list of variable value nodes *) 
val get_ids : value_node list -> ID.t list  

val get_fn_id : value_node -> FnId.t  
val get_fn_ids : value_node list -> FnId.t list 

(***
    helpers for values 
 ***)

val mk_val : ?src:SrcInfo.t -> ?ty:Type.t -> value -> value_node

val mk_var : ?src:SrcInfo.t -> ?ty:Type.t -> ID.t -> value_node 
val mk_op :  ?src:SrcInfo.t -> ?ty:Type.t -> Prim.prim -> value_node 

val mk_globalfn : ?src:SrcInfo.t -> ?ty:Type.t -> FnId.t -> value_node

val mk_num : ?src:SrcInfo.t -> ?ty:Type.t -> ParNum.t -> value_node
    
val mk_bool : ?src:SrcInfo.t -> bool -> value_node 
val mk_int32  : ?src:SrcInfo.t -> int -> value_node

val mk_float32 : ?src:SrcInfo.t -> float -> value_node
val mk_float64 : ?src:SrcInfo.t -> float -> value_node 
(*** 
    helpers for expressions 
 ***) 

val map_default_types : 
      Type.t list option -> value_node list -> Type.t list 
  
val mk_app :
     ?src:SrcInfo.t -> ?types:Type.t list -> value_node -> 
      value_node list -> exp_node 

val mk_primapp : 
     ?src:SrcInfo.t -> Prim.prim -> output_types:Type.t list -> 
       value_node list -> exp_node  

val mk_arr :
      ?src:SrcInfo.t -> ?types:Type.t list -> value_node list -> exp_node
 
val mk_val_exp : ?src:SrcInfo.t -> ?ty:Type.t -> 
      value -> exp_node

val mk_vals_exp :
      ?src:SrcInfo.t -> ?types : Type.t list -> value list -> exp_node

val mk_cast : ?src:SrcInfo.t -> Type.t -> value_node -> exp_node
val mk_exp :  ?src:SrcInfo.t -> ?types:Type.t list -> exp -> exp_node
val mk_call : 
      ?src:SrcInfo.t -> FnId.t -> Type.t list  -> value_node list ->
         exp_node 
val mk_map : ?src:SrcInfo.t -> closure -> value_node list -> exp_node 
val mk_reduce : 
      ?src:SrcInfo.t -> 
        closure -> closure -> 
          value_node list -> value_node list -> exp_node
val mk_scan : 
      ?src:SrcInfo.t -> 
        closure -> closure -> 
          value_node list -> value_node list -> exp_node
          
val mk_closure : fn -> value_node list -> closure 

val empty_stmt : stmt_node 
val is_empty_stmt : stmt_node -> bool 

val mk_phi : 
     ?src:SrcInfo.t -> ?ty:Type.t -> 
       ID.t -> value_node -> value_node -> phi_node

val empty_phi : phi_node 
val is_empty_phi : phi_node -> bool 
       
val mk_phi_nodes : ID.t list -> ID.t list -> ID.t list -> phi_nodes  

val mk_phi_nodes_from_values 
      : value_node list -> value_node list -> value_node list -> phi_nodes
       
val collect_phi_values :bool -> phi_nodes -> ID.t list * value_node list 
