open Imp

type block_info = {
    (*stmts : Imp.stmt list;*)
    block_ids : ID.t list;
    block_types : ImpType.t ID.Map.t;
    block_shapes : SymbolicShape.t ID.Map.t;
    block_storages : Imp.storage ID.Map.t;
}

class builder : object
  method declare : ID.t -> ?storage:Imp.storage ->
    ?shape:SymbolicShape.t -> ImpType.t -> unit
  method fresh_local_id : ?name:string -> ?storage:Imp.storage ->
    ?shape:SymbolicShape.t -> ImpType.t -> ID.t
  method fresh_local : ?name:string -> ?storage:Imp.storage ->
    ?shape:SymbolicShape.t -> ImpType.t -> value_node

  method var : ID.t -> value_node
  method cast : value_node -> ImpType.t -> value_node * (stmt list)
  method info : block_info

  method get_shape : ID.t -> SymbolicShape.t
  method get_type : ID.t -> ImpType.t
  method get_storage : ID.t -> Imp.storage
end

class fn_builder : object
  inherit builder
  method declare_input : ID.t ->  ImpType.t -> unit
  method fresh_input : ImpType.t -> value_node
  method declare_output : ID.t -> ?shape:SymbolicShape.t -> ImpType.t -> unit
  method fresh_output : ?shape:SymbolicShape.t -> ImpType.t -> value_node
  method finalize_fn : ?name:string -> Imp.block ->  fn
end
