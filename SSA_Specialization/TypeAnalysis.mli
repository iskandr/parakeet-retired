
module type TYPE_ANALYSIS_PARAMS = sig 
  val interpState : InterpState.t
  val closures : (ID.t, value) Hashtbl.t
  val closureArgs : (ID.t, ID.t list) Hashtbl.t
  val closureArity : (ID.t, int) Hashtbl.t
  val specialize : value -> Signature.t -> fundef   
end

module Make : functor (T: TYPE_ANALYSIS_PARAMS) -> SSA_Analysis.EVALUATOR