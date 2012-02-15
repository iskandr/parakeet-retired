

val translate_kernel 
  :  Imp.fn -> ?dataLayouts:GpuVal.data_layout array ->PtxVal.ptx_space array -> 
     Ptx.kernel * PtxCallingConventions.calling_conventions 