open SSA


val mk_fn :
      ?name:string ->
      ?tenv:tenv -> input_ids:ID.t list -> output_ids:ID.t list ->
        body:block -> fn
(***
     helpers for statements
 ***)

val stmt : ?src:SrcInfo.t -> ?id:StmtId.t -> stmt -> stmt_node
val set : ?src:SrcInfo.t -> ID.t list -> exp_node -> stmt_node
val setidx :
      ?src:SrcInfo.t -> value_node -> value_nodes -> value_node -> stmt_node


(* get the id of a variable value node *)
val get_id : value_node -> ID.t

(* get the ids from a list of variable value nodes *)
val get_ids : value_node list -> ID.t list

val get_fn_id : value_node -> FnId.t
val get_fn_ids : value_node list -> FnId.t list

(***
    helpers for values
 ***)

val wrap_value : ?src:SrcInfo.t -> ?ty:Type.t -> value -> value_node

val var : ?src:SrcInfo.t -> ?ty:Type.t -> ID.t -> value_node
val op :  ?src:SrcInfo.t -> ?ty:Type.t -> Prim.t -> value_node

val globalfn : ?src:SrcInfo.t -> ?ty:Type.t -> FnId.t -> value_node

val num : ?src:SrcInfo.t -> ?ty:Type.t -> ParNum.t -> value_node

val bool : ?src:SrcInfo.t -> bool -> value_node
val int32  : ?src:SrcInfo.t -> int -> value_node

val float32 : ?src:SrcInfo.t -> float -> value_node
val float64 : ?src:SrcInfo.t -> float -> value_node

val is_const : value_node -> bool
val is_const_int : value_node -> bool
val get_const : value_node -> ParNum.t
val get_const_int : value_node -> int

(***
    helpers for expressions
 ***)

val map_default_types :
      Type.t list option -> value_node list -> Type.t list

val app : ?src:SrcInfo.t -> value_node -> value_node list -> exp_node

val primapp :
     ?src:SrcInfo.t -> Prim.t -> output_types:Type.t list ->
       value_node list -> exp_node

val arr :
      ?src:SrcInfo.t -> ?types:Type.t list -> value_node list -> exp_node

val val_exp : ?src:SrcInfo.t -> ?ty:Type.t ->
      value -> exp_node

val vals_exp :
      ?src:SrcInfo.t -> ?types : Type.t list -> value list -> exp_node

val cast : ?src:SrcInfo.t -> Type.t -> value_node -> exp_node
val exp :  ?src:SrcInfo.t -> ?types:Type.t list -> exp -> exp_node
val call :
      ?src:SrcInfo.t -> FnId.t -> Type.t list  -> value_node list ->
         exp_node

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

val phi :
     ?src:SrcInfo.t -> ?ty:Type.t ->
       ID.t -> value_node -> value_node -> phi_node

val empty_phi : phi_node
val is_empty_phi : phi_node -> bool

val phi_nodes : ID.t list -> ID.t list -> ID.t list -> phi_nodes

val phi_nodes_from_values
      : value_node list -> value_node list -> value_node list -> phi_nodes

val collect_phi_values :bool -> phi_nodes -> ID.t list * value_node list

val types_of_value_nodes : value_node list -> Type.t list




val fn_builder :
  ?name:string -> input_types:Type.t list -> output_types:Type.t list ->
    ?local_types:Type.t list ->
      (value_nodes * value_nodes * value_nodes -> stmt_node list) -> fn

val untyped_fn1_builder :
  (value_node -> value_node -> stmt_node list) -> fn

val untyped_fn2_builder :
  (value_node -> value_node -> value_node -> stmt_node list) -> fn