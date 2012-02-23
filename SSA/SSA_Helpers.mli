open SSA

(*
(***
     helpers for statements
 ***)


(* get the id of a variable value node *)
val get_id : value_node -> ID.t

(* get the ids from a list of variable value nodes *)
val get_ids : value_node list -> ID.t list

val get_fn_id : value_node -> FnId.t
val get_fn_ids : value_node list -> FnId.t list


(***
    helpers for expressions
 ***)

val map_default_types :
      Type.t list option -> value_node list -> Type.t list



val closure : fn -> value_node list -> closure


val (<--) : value_node list -> exp_node -> stmt_node
val (@@) : value_node -> value_node list -> exp_node
val scalar_op : Prim.scalar_op -> value_node
val array_op : Prim.array_op -> value_node
val impure_op : Prim.impure_op -> value_node

val print : value_node

val inf : value_node
val neginf : value_node

val plus : value_node
val minus : value_node
val mul : value_node
val div : value_node

val lt : value_node
val lte : value_node
val eq : value_node

val one : value_node
val neg_one : value_node
val zero : value_node

val reduce : value_node
val map : value_node
val allPairs : value_node

(*val where : value_node*)
val index : value_node
(*val til : value_node*)
val find : value_node
val dimsize : value_node
val select : value_node


val len : value_node -> exp_node
val value : value_node -> exp_node
val values : value_node list -> exp_node

val incr : ID.t -> value_node -> stmt_node
val set_int : ID.t -> Int32.t -> stmt_node

val empty_stmt : stmt_node
val is_empty_stmt : stmt_node -> bool

val empty_phi : phi_node
val is_empty_phi : phi_node -> bool

val phi_nodes : ID.t list -> ID.t list -> ID.t list -> phi_nodes


val types_of_value_nodes : value_node list -> Type.t list






val untyped_fn1_builder :
  (value_node -> value_node -> stmt_node list) -> fn

val untyped_fn2_builder :
  (value_node -> value_node -> value_node -> stmt_node list) -> fn
*)