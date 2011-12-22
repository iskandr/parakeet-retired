open Imp 

type block_info = { 
    stmts : Imp.stmt list;
    block_ids : ID.t list;  
    block_types : ImpType.t ID.Map.t; 
    block_shapes : SymbolicShape.t ID.Map.t; 
}

class codegen : object 
  method fresh_id : ?shape:SymbolicShape.t -> ImpType.t -> ID.t 
  method var : ?shape:SymbolicShape.t -> ImpType.t -> value_node 
  method var_exp : ?shape:SymbolicShape.t -> ImpType.t -> exp_node 
  method set_id : ID.t -> exp_node -> unit 
  method set : value_node -> exp_node -> unit 
  method set_val : value_node -> value_node -> unit 
  method cast : value_node -> ImpType.t -> value_node 
  method finalize_block : block_info      
end

class fn_codegen : object 
  inherit codegen 
  method named_input : ID.t -> ?shape:SymbolicShape.t -> ImpType.t -> value_node  
  method fresh_input : ?shape:SymbolicShape.t -> ImpType.t -> value_node 
  method named_output : ID.t -> ?shape:SymbolicShape.t -> ImpType.t -> value_node
  method fresh_output : ?shape:SymbolicShape.t -> ImpType.t -> value_node
  method finalize_fn :  fn 
end
