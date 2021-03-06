open Imp

type fn_info = {
  mutable ids : ID.Set.t;
  mutable types : ImpType.t ID.Map.t;
  mutable shapes : SymbolicShape.t ID.Map.t;
  mutable storages : Imp.storage ID.Map.t;
}

val create_fn_info  : unit -> fn_info

val id_of : value_node -> ID.t

class builder : fn_info -> object
  method info : fn_info
  method clone : builder
  method to_list : stmt list
  method has_id : ID.t -> bool
  method get_type : ID.t -> ImpType.t
  method get_shape : ID.t -> SymbolicShape.t
  method get_storage :ID.t -> Imp.storage
  method value_shape : value_node -> SymbolicShape.t
  method value_storage : value_node -> Imp.storage
  method append : stmt -> unit
  method concat_list : stmt list -> unit
  method concat : builder -> unit



  method mk_temp : value_node -> value_node
  method flatten_simple : value_node -> value_node
  method flatten_lhs : value_node -> value_node
  method flatten_rhs : value_node -> value_node
  method flatten : stmt -> stmt

  method declare :
    ID.t -> ?storage:Imp.storage -> ?shape:SymbolicShape.t -> ImpType.t -> unit

  method cast : string -> value_node -> ImpType.t -> value_node
  method fresh_local :
    string ->  ?storage:Imp.storage -> ?shape:SymbolicShape.t -> ImpType.t ->
      value_node

  method var :    ID.t -> value_node
  method fixdims :
    arr:value_node -> dims:int list -> indices:value_nodes -> value_node
  method idx_or_fixdims :
    arr:value_node -> dims:value_nodes -> indices:value_nodes -> value_node

  method build_add : string -> value_node -> value_node -> value_node

  method inline :
    ?call_by_copy:bool -> Imp.fn -> value_nodes -> value_nodes -> unit

end

val (+=) : builder -> stmt -> unit

class fn_builder : string -> object
  method declare_input : ID.t ->  ImpType.t -> unit
  method fresh_input : ImpType.t -> value_node
  method declare_output : ID.t -> ?shape:SymbolicShape.t -> ImpType.t -> unit
  method fresh_output : ?shape:SymbolicShape.t -> ImpType.t -> value_node
  method finalize_fn : fn

  method body : builder
end