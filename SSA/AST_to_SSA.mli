open Base
(* environment mapping strings to SSA IDs or global function IDs *)
module Env : sig
  type t =
    | GlobalScope of (string -> FnId.t)
    | LocalScope of (ID.t String.Map.t) * t

  val mem : t -> string -> bool
  val add : t -> string -> ID.t -> t

  val add_list : t -> string list -> ID.t list -> t
  val extend : t -> string list -> ID.t list -> t
  val lookup : t -> string -> SSA.Untyped.value
  val lookup_id : t -> string -> ID.t
  val lookup_opt : t -> string -> SSA.Untyped.value option
  val lookup_id_opt : t -> string -> ID.t option
  val has_id : t -> string -> bool
end



val flatten_indexing : AST.node -> AST.node list
val translate_exp :
      Env.t -> SSA_Codegen.codegen -> AST.node -> SSA.Untyped.exp_node

val translate_value :
      Env.t -> SSA_Codegen.codegen -> AST.node -> SSA.Untyped.value_node
val translate_values :
      Env.t -> SSA_Codegen.codegen ->
        AST.node list -> SSA.Untyped.value_node list


val exp_as_value :
  Env.t -> SSA_Codegen.codegen -> string -> AST.node -> SSA.Untyped.value_node

val exps_as_values :
  Env.t -> SSA_Codegen.codegen -> string -> AST.node list ->
    SSA.Untyped.value_node list

val translate_assignment :
  Env.t -> SSA_Codegen.codegen -> AST.node list -> AST.node -> Env.t


val translate_stmt :
      Env.t -> SSA_Codegen.codegen -> ID.t list -> AST.node -> Env.t

val translate_loop_body :
      Env.t -> SSA_Codegen.codegen -> ID.t list ->  AST.node ->
        SSA.Untyped.phi_node list * Env.t

val translate_block :
      Env.t -> SSA_Codegen.codegen -> ID.t list -> AST.node list -> Env.t

val translate_fn :
  ?name:string -> Env.t -> string list -> AST.node -> SSA.Untyped.fn
