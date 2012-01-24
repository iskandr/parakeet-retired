open Base 
open Llvm_executionengine


val parnum_to_generic : ParNum.t -> GenericValue.t 
val to_llvm : Ptr.t Value.t -> GenericValue.t 
val to_llvm_pointer : Ptr.t Value.t -> GenericValue.t 
val delete_llvm_gv : GenericValue.t -> ImpType.t -> unit  
