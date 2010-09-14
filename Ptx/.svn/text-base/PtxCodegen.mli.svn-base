class ptx_codegen :
  object ('a)
    method convert :
      destType:Ptx.ty -> srcType:Ptx.ty -> srcReg:Ptx.value -> Ptx.value
    method declare_arg : ID.t -> DynType.t -> Ptx.value
    method declare_local : ID.t -> DynType.t -> Ptx.value
    method declare_shared_slice : ID.t -> Ptx.value
    method emit : Ptx.instruction list -> unit
    method finalize_kernel : Ptx.kernel
    method fresh_label : Ptx.symid
    method fresh_reg : Ptx.ty -> Ptx.value
    method fresh_regs : Ptx.ty -> int -> Ptx.value array
    method get_data_reg : ID.t -> Ptx.value
    method get_dyn_type : ID.t -> DynType.t
    method get_shape_reg : ID.t -> Ptx.value
    method init_reg : Ptx.ty -> Ptx.value -> Ptx.value
    method run_rewrite_pass :
      ('a -> Ptx.instruction -> Ptx.instruction list) -> unit
end
