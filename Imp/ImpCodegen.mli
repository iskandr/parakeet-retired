
class type fn_state_interface = object 
  method has_type : ID.t -> bool 
  method get_type : ID.t -> DynType.t
  
  method get_id_shape : ID.t -> SymbolicShape.shape
  method get_exp_shape : Imp.exp_node -> SymbolicShape.shape
  method set_shape : ID.t -> SymbolicShape.shape -> unit
       
  method is_shared : ID.t -> bool  
  method is_array : ID.t -> bool   

  method has_array_storage : ID.t -> bool 
  method get_array_storage : ID.t -> Imp.array_storage
  method set_array_storage : ID.t -> Imp.array_storage -> unit
   
  method fresh_id : DynType.t -> ID.t 
  
  method fresh_local_id : 
      ?dims:(Imp.exp_node list) -> ?storage:Imp.array_storage -> 
        DynType.t -> ID.t
         
  method add_to_locals : ID.t -> unit 
  
  method fresh_var : 
      ?dims:(Imp.exp_node list) -> ?storage:Imp.array_storage -> 
        DynType.t -> Imp.exp_node
  method fresh_vars : int -> DynType.t -> Imp.exp_node array
    
  method fresh_input_id : DynType.t -> ID.t 
  method fresh_input : DynType.t -> Imp.exp_node 
    
  method fresh_output_id : ?dims:(Imp.exp_node list) -> DynType.t -> ID.t 
  method fresh_output : ?dims:(Imp.exp_node list) -> DynType.t -> Imp.exp_node 
    
  method shared_vec_id : DynType.t -> int list -> ID.t 
  method shared_vec_var : DynType.t -> int list -> Imp.exp_node    
end

class code_buffer : fn_state_interface ->  object 
  method emit : Imp.block -> unit
   
  method splice : 
    Imp.fn -> Imp.exp_node array -> Imp.exp_node array -> Imp.block -> Imp.block
     
  method splice_emit : 
    Imp.fn -> Imp.exp_node array -> Imp.exp_node array -> Imp.block -> unit
    
  method to_block : Imp.block
end

and fn_state : object   
  method finalize : Imp.fn

  method fresh_code_buffer : code_buffer 
  method main_code_buffer : code_buffer
  
  method has_type : ID.t -> bool 
  method get_type : ID.t -> DynType.t
  
  method get_id_shape : ID.t -> SymbolicShape.shape
  method get_exp_shape : Imp.exp_node -> SymbolicShape.shape
  method set_shape : ID.t -> SymbolicShape.shape -> unit
       
  method is_shared : ID.t -> bool  
  method is_array : ID.t -> bool   

  method has_array_storage : ID.t -> bool 
  method get_array_storage : ID.t -> Imp.array_storage
  method set_array_storage : ID.t -> Imp.array_storage -> unit
   
  method fresh_id : DynType.t -> ID.t 
  
  method fresh_local_id : 
      ?dims:(Imp.exp_node list) -> ?storage:Imp.array_storage -> 
        DynType.t -> ID.t
         
  method add_to_locals : ID.t -> unit 
  
  method fresh_var : 
      ?dims:(Imp.exp_node list) -> ?storage:Imp.array_storage -> 
        DynType.t -> Imp.exp_node
  method fresh_vars : int -> DynType.t -> Imp.exp_node array
    
  method fresh_input_id : DynType.t -> ID.t 
  method fresh_input : DynType.t -> Imp.exp_node 
    
  method fresh_output_id : ?dims:(Imp.exp_node list) -> DynType.t -> ID.t 
  method fresh_output : ?dims:(Imp.exp_node list) -> DynType.t -> Imp.exp_node 
    
  method shared_vec_id : DynType.t -> int list -> ID.t 
  method shared_vec_var : DynType.t -> int list -> Imp.exp_node      

end 
 
