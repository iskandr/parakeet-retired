(********** VALUES **********)
type value = 
  | Var of ID.t
  | Num of ParNum.t 
  | Str of string
  | Sym of string
  | Unit
  | Prim of Prim.prim
  | GlobalFn of FnId.t  

and value_node = { 
  value_type : Type.t;
  value_src : SourceInfo.t option; 
  value : value 
}
and value_nodes = value_node list   

type axes = int list 

type closure = {   
  closure_fn: FnId.t; 
  closure_args: value_node list; 
  closure_arg_types: Type.t list; 
  closure_input_types:Type.t list; 
  closure_output_types: Type.t list 
} 
        
(********** EXPRESSIONS **********)
type exp = 
  | App of  value_node * value_nodes
  | Arr of value_nodes
  | Values of value_nodes
  (* nodes below are only used after type specialization *) 
  | Cast of Type.t * value_node  
  | Call of FnId.t * value_nodes 
  | PrimApp of Prim.prim * value_nodes  
  | Map of closure * value_nodes
  | Reduce of closure * axes * value_nodes
  | Scan of closure * axes * value_nodes 
 
and exp_node = { 
  exp: exp; 
  exp_src : SourceInfo.t option;
  (* because a function applicatin might return multiple values,*)
  (* expressions have multiple types *)  
  exp_types : Type.t list; 
}

(********** STATEMENTS **********) 
type stmt = 
  | Set of ID.t list * exp_node 
  | SetIdx of ID.t * value_nodes * value_node
  | If of value_node * block * block * phi_nodes
  (* testBlock, testVal, body, loop header  *)  
  | WhileLoop of block * value_node * block * phi_nodes 
and stmt_node = { 
    stmt: stmt;
    stmt_src: SourceInfo.t option;
    stmt_id : StmtId.t;  
}
and block = stmt_node Block.t
and phi_node = { 
  phi_id : ID.t;
  phi_left:  value_node;
  phi_right: value_node;
  phi_type : Type.t; 
  phi_src : SourceInfo.t option; 
}  
and phi_nodes = phi_node list 


type fundef = {
  body: block;
  tenv : tenv;
  input_ids:ID.t list;
  output_ids: ID.t list; 
  fn_input_types : Type.t list;
  fn_output_types : Type.t list;  
  fn_id : FnId.t; 
}
and tenv = Type.t ID.Map.t 

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
val closure_to_str : closure -> string 

val extract_nested_map_fn_id : fundef -> FnId.t option 
    
val mk_fundef : 
      ?tenv:tenv -> input_ids:ID.t list -> output_ids:ID.t list -> 
        body:block -> fundef 
(***
     helpers for statements 
 ***) 

val mk_stmt : ?src:SourceInfo.t -> ?id:StmtId.t -> stmt -> stmt_node 
val mk_set : ?src:SourceInfo.t -> ID.t list -> exp_node -> stmt_node 
      
(* get the id of a variable value node *) 
val get_id : value_node -> ID.t 

(* get the ids from a list of variable value nodes *) 
val get_ids : value_node list -> ID.t list  

val get_fn_id : value_node -> FnId.t  
val get_fn_ids : value_node list -> FnId.t list 

(***
    helpers for values 
 ***)

val mk_val : ?src:SourceInfo.t -> ?ty:Type.t -> value -> value_node

val mk_var : ?src:SourceInfo.t -> ?ty:Type.t -> ID.t -> value_node 
val mk_op :  ?src:SourceInfo.t -> ?ty:Type.t -> Prim.prim -> value_node 

val mk_globalfn : ?src:SourceInfo.t -> ?ty:Type.t -> FnId.t -> value_node

val mk_num : ?src:SourceInfo.t -> ?ty:Type.t -> ParNum.t -> value_node
    
val mk_bool : ?src:SourceInfo.t -> bool -> value_node 
val mk_int32  : ?src:SourceInfo.t -> int -> value_node
val mk_float32 : ?src:SourceInfo.t -> float -> value_node

(*** 
    helpers for expressions 
 ***) 

val map_default_types : 
      Type.t list option -> value_node list -> Type.t list 
  
val mk_app :
     ?src:SourceInfo.t -> ?types:Type.t list -> value_node -> 
      value_node list -> exp_node 

val mk_primapp : 
     ?src:SourceInfo.t -> Prim.prim -> Type.t list -> 
       value_node list -> exp_node  

val mk_arr :
      ?src:SourceInfo.t -> ?types:Type.t list -> value_node list -> exp_node
 
val mk_val_exp : ?src:SourceInfo.t -> ?ty:Type.t -> 
      value -> exp_node

val mk_vals_exp :
      ?src:SourceInfo.t -> ?types : Type.t list -> value list -> exp_node

val mk_cast : ?src:SourceInfo.t -> Type.t -> value_node -> exp_node
val mk_exp :  ?src:SourceInfo.t -> ?types:Type.t list -> exp -> exp_node
val mk_call : 
      ?src:SourceInfo.t -> FnId.t -> Type.t list  -> value_node list ->
         exp_node 
val mk_map : ?src:SourceInfo.t -> closure -> value_node list -> exp_node 
val mk_reduce : 
      ?src:SourceInfo.t -> 
        closure -> closure -> 
          value_node list -> value_node list -> exp_node
val mk_scan : 
      ?src:SourceInfo.t -> 
        closure -> closure -> 
          value_node list -> value_node list -> exp_node
          
val mk_closure : fundef -> value_node list -> closure 

val empty_stmt : stmt_node 
val is_empty_stmt : stmt_node -> bool 

val mk_phi : 
     ?src:SourceInfo.t -> ?ty:Type.t -> 
       ID.t -> value_node -> value_node -> phi_node

val empty_phi : phi_node 
val is_empty_phi : phi_node -> bool 
       
val mk_phi_nodes : ID.t list -> ID.t list -> ID.t list -> phi_nodes  

val mk_phi_nodes_from_values 
      : value_node list -> value_node list -> value_node list -> phi_nodes
       
val collect_phi_values :bool -> phi_nodes -> ID.t list * value_node list 
