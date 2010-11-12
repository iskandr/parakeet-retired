
type tenv = DynType.t ID.Map.t 

type if_gate = { 
  if_output_ids : ID.t list;
  true_ids : ID.t list; 
  false_ids : ID.t list 
}

type loop_gate = { 
  (* what variables are assigned in the body of this loop? *) 
  loop_ids : ID.t list; 
  
  (* from which external identifiers do internal variables 
     get their initial values 
  *) 
  loop_inputs : ID.t ID.Map.t;
  
  (* what variables visible after this loop are generated, and
     from which internal var do they get their value?  
  *)
  loop_outputs : ID.t ID.Map.t;
  
  (* when repeating the loop body, which variables at the end of the
     body feed back into variables at the beginning? 
  *) 
  loop_header : ID.t ID.Map.t;  
}


type stmt = 
  | Set of ID.t list * exp_node 
  | Ignore of exp_node
  | SetIdx of ID.t * (value_node list) * value_node
  | If of value_node * block * block * if_gate
  | WhileLoop of exp_node * block * loop_gate 
and stmt_node = { 
    stmt: stmt;
    stmt_src: SourceInfo.source_info option;
    stmt_id : StmtId.t;  
}
and block = stmt_node list 
and  exp = 
       
  | App of  value_node * value_node list
  | ArrayIndex of value_node * value_node list
  | Arr of value_node list
  | Cast of DynType.t * value_node  
  | Values of value_node list 
and exp_node = { 
  exp: exp; 
  exp_src : SourceInfo.source_info option;
  exp_types : DynType.t list; 
} 
and value_node = { 
  value_type : DynType.t;
  value_src : SourceInfo.source_info option; 
  value : value 
}  
and value = 
  | Var of ID.t
  | GlobalFn of FnId.t  
  | Num of PQNum.num 
  | Str of string
  | Sym of string
  | Unit
  | Prim of Prim.prim
  | Lam of fundef
and fundef = {
  body: block;
  tenv : tenv;
  input_ids: ID.t list;
  output_ids: ID.t list; 
  fn_type : DynType.t; 
  fn_id : FnId.t; 
}


val is_simple_exp : exp -> bool 

val id_list_to_str : ID.t list -> string 

val typed_id_to_str : tenv -> ID.t -> string 
val typed_id_list_to_str : tenv -> ID.t list -> string 
  
        
val block_to_str : ?space:string -> ?tenv:tenv ->  block -> string  
val stmt_node_to_str : ?space:string -> ?tenv:tenv -> stmt_node -> string
val exp_to_str : exp_node -> string
val value_node_to_str : value_node -> string
val value_nodes_to_str : value_node list -> string 
  
val value_to_str : value -> string 
val value_node_list_to_str : ?sep:string -> value_node list -> string 
val value_list_to_str : ?sep:string -> value list -> string  
  
val fundef_to_str : fundef -> string  
  
val mk_fundef : 
      ?tenv:tenv -> input_ids:ID.t list -> output_ids:ID.t list -> 
        body:block -> fundef 
(***
     helpers for statements 
 ***) 

val mk_stmt : ?src:SourceInfo.source_info -> ?id:StmtId.t -> stmt -> stmt_node 
val mk_set : ?src:SourceInfo.source_info -> ID.t list -> exp_node -> stmt_node 
  
val mk_if : 
     ?src:SourceInfo.source_info -> value_node -> block -> block -> 
       if_gate -> stmt_node
      
(* get the id of a variable value node *) 
val get_id : value_node -> ID.t 

(* get the ids from a list of variable value nodes *) 
val get_ids : value_node list -> ID.t list  

val get_fn_id : value_node -> FnId.t  
val get_fn_ids : value_node list -> FnId.t list 

(***
    helpers for values 
 ***)

val mk_val : 
      ?src:SourceInfo.source_info -> ?ty:DynType.t -> value -> value_node

val mk_var : ?src:SourceInfo.source_info -> ?ty:DynType.t -> ID.t -> value_node 
val mk_op : 
      ?src:SourceInfo.source_info -> ?ty:DynType.t -> Prim.prim -> value_node 

val mk_globalfn : 
      ?src:SourceInfo.source_info -> ?ty:DynType.t -> FnId.t -> value_node

val mk_num :
     ?src:SourceInfo.source_info -> ?ty:DynType.t -> PQNum.num -> value_node
    
val mk_bool : ?src:SourceInfo.source_info -> bool -> value_node 
val mk_int32 
    : ?src:SourceInfo.source_info -> int -> value_node
val mk_float32
    : ?src:SourceInfo.source_info -> float -> value_node  
(*** 
    helpers for expressions 
 ***) 

val map_default_types : 
      DynType.t list option -> value_node list -> DynType.t list 
  
val mk_app :
     ?src:SourceInfo.source_info -> ?types:DynType.t list -> 
      value_node -> value_node list -> exp_node 

val mk_arr :
      ?src:SourceInfo.source_info ->
         ?types:DynType.t list -> value_node list -> exp_node
 
val mk_val_exp : ?src:SourceInfo.source_info -> ?ty:DynType.t -> 
      value -> exp_node

val mk_vals_exp :
      ?src:SourceInfo.source_info -> ?types : DynType.t list -> 
        value list -> exp_node
val mk_arr_idx : 
      ?src:SourceInfo.source_info -> ?types:DynType.t list -> 
        value_node -> value_node list -> exp_node 
        
val mk_cast : ?src:SourceInfo.source_info -> DynType.t -> value_node -> exp_node
val mk_exp : 
      ?src:SourceInfo.source_info -> ?types:DynType.t list -> exp -> exp_node
  