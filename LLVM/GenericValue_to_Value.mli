
open Llvm_executionengine

val generic_value_to_parnum : GenericValue.t -> Type.elt_t -> ParNum.t 
val of_generic_value : ?boxed_scalars:bool -> GenericValue.t -> ImpType.t -> Ptr.t Value.t 


