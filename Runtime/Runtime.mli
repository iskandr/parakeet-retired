type value = DataId.t Value.t
type values = value list

module type SCHEDULER = sig
  val call : TypedSSA.fn -> values -> values
  val adverb : (TypedSSA.fn, values, int list) Adverb.info -> values
  val array_op : Prim.array_op -> values -> values
end

module type INTERP = sig
  val eval_adverb : (TypedSSA.fn, values, int list) Adverb.info -> values
  val eval_call : TypedSSA.fn -> value list -> value list
  val eval_exp : TypedSSA.exp_node -> value list
end

module Scheduler : SCHEDULER
module Interp : INTERP


(* evaluates a function applied to a set of arguments on the host *)
val call : TypedSSA.fn -> Ptr.t Value.t list -> Ptr.t Value.t list