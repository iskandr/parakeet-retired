

class ptx_codegen : object
  method emit : Ptx.instruction list -> unit 
  method finalize_kernel :
           Ptx.kernel * PtxCallingConventions.calling_conventions  
          
  method fresh_label : int
  method cached : ?ty:PtxType.ty -> PtxVal.value -> PtxVal.value 
  
  method fresh_reg : PtxType.ty -> PtxVal.value 
  method fresh_regs : PtxType.ty -> int -> PtxVal.value array 
  
  method convert : destReg:PtxVal.value -> srcVal:PtxVal.value -> unit
  method convert_fresh 
         : destType:PtxType.ty -> srcVal:PtxVal.value -> PtxVal.value 

   
  method is_shared_ptr : PtxVal.value -> bool 
  method get_shared_dims : PtxVal.value -> int array
  method get_shape_reg : PtxVal.value -> PtxVal.value
  
  method get_tex_ref : PtxVal.value -> PtxVal.value
  method is_tex : PtxVal.value -> bool
  method get_tex_geom : PtxVal.value -> Ptx.geom
  
  method is_global_array_ptr : PtxVal.value -> bool  
  method get_global_array_rank : PtxVal.value -> int
  
  method get_array_rank : PtxVal.value -> int 
  
  method imp_dyn_type : ID.t -> DynType.t 
  method imp_reg : ID.t -> PtxVal.value
   
  method declare_shared_vec : ID.t -> DynType.t -> int list -> PtxVal.value 
  method declare_slice : PtxVal.value -> PtxVal.value -> unit 
  
  (* a scalar local *) 
  method declare_local : ID.t -> DynType.t -> PtxVal.value 
  (* a local which needs heap storage *) 
  method declare_storage_arg : ID.t -> DynType.t -> PtxVal.value
  method declare_input : ID.t -> DynType.t -> PtxVal.ptx_space -> PtxVal.value 
  method declare_output : ID.t -> DynType.t -> PtxVal.value 
  
  method compute_threads_per_block : PtxVal.value 
  method compute_linear_block_index : PtxVal.value 
  method compute_linear_block_offset : PtxVal.value
  (* which thread are you in the entire computation grid? *)  
  method compute_linear_thread_index  : PtxVal.value
  
  (* given a base register, the size of the contained elements, and an array 
     of index registers, compute a location in a data array 
  *)  
  method compute_address :
    PtxVal.value -> int -> PtxVal.value array -> PtxVal.value
  
  (* assumes that register is in this codegen's symbols table *) 
  method value_to_str : PtxVal.value -> string  
end


  