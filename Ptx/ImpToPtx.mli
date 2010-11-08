

val translate_kernel 
  : ?input_spaces:PtxVal.ptx_space array -> Imp.fn -> 
     Ptx.kernel * PtxCallingConventions.calling_conventions 