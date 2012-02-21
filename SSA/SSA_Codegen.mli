(* The codegen object helps keep track of information about an SSA function*)
(* function and its variables as we are in the process of constructing it. *)
(* Should be used only from the AST_to_SSA module *)


class codegen : object
    val types : Type.t ID.Map.t ref
    val code : stmt_node DynArray.t

    method get_type_env : Type.t ID.Map.t
    method get_type  : ID.t -> Type.t
    method add_type : ID.t -> Type.t -> unit

    method fresh_var  : Type.t -> ID.t
    method id_value_node : ID.t -> value_node

    method cvt : to_type:Type.t->from_type:Type.t->value_node->value_node

    method cvt_list
      : to_type : Type.t -> from_types:Type.t list ->
          value_node list ->value_node list

    method emit : stmt_node list -> unit

    method finalize : SSA.block
end

val mk_codegen_fn
      : Type.t list ->  Type.t list ->
        (codegen -> value_node list-> value_node list -> unit) -> UntypedSSA.fn




