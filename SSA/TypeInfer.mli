
(* given a first-order op and its arguments, infer the return type *)
val infer_scalar_op : Prim.scalar_op -> DynType.t list -> DynType.t 
val infer_binop : Prim.scalar_op -> DynType.t -> DynType.t ->  DynType.t 
val infer_unop : Prim.scalar_op -> DynType.t ->  DynType.t
(* first order array operators take data arguments and return only one value *)  
val infer_simple_array_op : Prim.array_op -> DynType.t list -> DynType.t

(* adverbs take both functions and data, returning possibly multiple 
   arguments 
*) 
val infer_adverb : Prim.array_op -> DynType.t list -> DynType.t list  

(* to which types must inputs be cast for an operator to work? *) 
val required_scalar_op_types : Prim.scalar_op -> DynType.t list -> DynType.t list