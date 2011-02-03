
module type GPU_RUNTIME_PARAMS = sig 
  val fnTable : FnTable.t 
  val memState : MemoryState.t 
end

type value = GpuVal.gpu_val 
type values = value list 

module Mk(P:GPU_RUNTIME_PARAMS) : sig
  val map : payload:SSA.fundef ->  closureArgs:values -> args:values -> values 
  val reduce : 
    init:SSA.fundef -> initClosureArgs:values ->
    payload:SSA.fundef -> payloadClosureArgs:values -> 
    args:values -> values  
  
  val where : value -> value 
  val index : value -> value -> value   
end 

              
