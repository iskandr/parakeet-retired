(********** VALUES **********)
type value = 
  | Var of ID.t
  | Num of PQNum.num 
  | Str of string
  | Sym of string
  | Unit
  | Prim of Prim.prim
  | GlobalFn of FnId.t  

and value_node = { 
  value_type : DynType.t;
  value_src : SourceInfo.t option; 
  value : value 
}
and value_nodes = value_node list   
    
(********** EXPRESSIONS **********)
type exp = 
  | App of  value_node * value_nodes
  | Arr of value_nodes
  | Values of value_nodes
  (* nodes below are only used after type specialization *) 
  | Cast of DynType.t * value_node  
  | Call of FnId.t * value_nodes 
  | PrimApp of Prim.prim * value_nodes  
  | Map of closure * value_nodes
  | Reduce of closure * closure * value_nodes   
  | Scan of closure * closure * value_nodes
and exp_node = { 
  exp: exp; 
  exp_src : SourceInfo.t option;
  (* because a function applicatin might return multiple values,*)
  (* expressions have multiple types *)  
  exp_types : DynType.t list; 
} 
(*
and typed_fn = { 
  fn_id : FnId.t; 
  fn_input_types : DynType.t list; 
  fn_output_types : DynType.t list;   
} 
*)
(*
and typed_prim = { 
  prim: Prim.prim;
  prim_type_index : DynType.t;  
} 
*)
and closure = {   
  closure_fn: FnId.t; 
  closure_args: value_node list; 
  closure_arg_types: DynType.t list; 
  closure_input_types:DynType.t list; 
  closure_output_types: DynType.t list 
} 

(********** STATEMENTS **********) 
type stmt = 
  | Set of ID.t list * exp_node 
  | SetIdx of ID.t * value_nodes * value_node
  | If of value_node * block * block * if_gate
  | WhileLoop of block * value_node * block * loop_gate  
and stmt_node = { 
    stmt: stmt;
    stmt_src: SourceInfo.t option;
    stmt_id : StmtId.t;  
}
and block = stmt_node array
and if_gate = { 
  if_output_ids : ID.t list;
  true_ids : ID.t list; 
  false_ids : ID.t list 
}
and loop_gate = { 
  (* what variables visible after this loop are generated, and
     from which internal var do they get their value?  
  *)
  loop_outputs : ID.t list; 
  
  (* what variables are assigned in the body of this loop? *) 
  loop_local_defs : ID.t list;
  
  (* every loop local variable gets its value either from above the 
     loop on the first iteration, or from a further loop variable on 
     a repeat iteration  
  *) 
  loop_header_map : (ID.t * ID.t) ID.Map.t;
  (* every loop output can either come a loop variable or from some
     variable preceding the loop (including, presumably, undefined)
  *)
  loop_output_map : (ID.t * ID.t) ID.Map.t;  
}

type fundef = {
  body: block;
  tenv : tenv;
  input_ids:ID.t list;
  output_ids: ID.t list; 
  fn_input_types : DynType.t list;
  fn_output_types : DynType.t list;  
  fn_id : FnId.t; 
}
and tenv = DynType.t ID.Map.t 

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
  
val mk_if : 
     ?src:SourceInfo.t -> value_node -> block -> block -> 
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

val mk_val : ?src:SourceInfo.t -> ?ty:DynType.t -> value -> value_node

val mk_var : ?src:SourceInfo.t -> ?ty:DynType.t -> ID.t -> value_node 
val mk_op :  ?src:SourceInfo.t -> ?ty:DynType.t -> Prim.prim -> value_node 

val mk_globalfn : ?src:SourceInfo.t -> ?ty:DynType.t -> FnId.t -> value_node

val mk_num : ?src:SourceInfo.t -> ?ty:DynType.t -> PQNum.num -> value_node
    
val mk_bool : ?src:SourceInfo.t -> bool -> value_node 
val mk_int32  : ?src:SourceInfo.t -> int -> value_node
val mk_float32 : ?src:SourceInfo.t -> float -> value_node

(*** 
    helpers for expressions 
 ***) 

val map_default_types : 
      DynType.t list option -> value_node list -> DynType.t list 
  
val mk_app :
     ?src:SourceInfo.t -> ?types:DynType.t list -> value_node -> 
      value_node list -> exp_node 

val mk_primapp : 
     ?src:SourceInfo.t -> Prim.prim -> DynType.t list -> 
       value_node list -> exp_node  

val mk_arr :
      ?src:SourceInfo.t -> ?types:DynType.t list -> value_node list -> exp_node
 
val mk_val_exp : ?src:SourceInfo.t -> ?ty:DynType.t -> 
      value -> exp_node

val mk_vals_exp :
      ?src:SourceInfo.t -> ?types : DynType.t list -> value list -> exp_node

val mk_cast : ?src:SourceInfo.t -> DynType.t -> value_node -> exp_node
val mk_exp :  ?src:SourceInfo.t -> ?types:DynType.t list -> exp -> exp_node
val mk_call : 
      ?src:SourceInfo.t -> FnId.t -> DynType.t list  -> value_node list ->
         exp_node 
val mk_map : ?src:SourceInfo.t -> closure -> value_node list -> exp_node 
val mk_reduce : 
      ?src:SourceInfo.t -> closure -> closure -> value_node list -> exp_node
val mk_scan : 
      ?src:SourceInfo.t -> closure -> closure -> value_node list -> exp_node
val mk_closure : fundef -> value_node list -> closure 

val empty_stmt : stmt_node 
val is_empty_stmt : stmt_node -> bool 
val empty_block : block 
val block_of_stmt : stmt_node -> block 
val block_append : block -> block -> block  
val block_concat : block list -> block 
val insert_stmt_after_block : block -> stmt_node -> block 
val insert_stmt_before_block : stmt_node -> block -> block  
val block_of_list : stmt_node list -> block  
val block_length : block -> int 
val block_idx : block -> int -> stmt_node
val block_iter_forward : (stmt_node -> unit) -> block -> unit
val block_iter_backward : (stmt_node -> unit) -> block -> unit
val block_fold_forward : ('a -> stmt_node -> 'a) -> 'a -> block -> 'a
val block_fold_backward : ('a -> stmt_node -> 'a) -> 'a -> block -> 'a
val block_for_all : (stmt_node -> bool) -> block -> bool  
