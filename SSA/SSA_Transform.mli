open Base 
open SSA 

type 'a update =
  | NoChange 
  | Update of 'a 
  | UpdateWithStmts of 'a * (stmt_node list) 

module type TRANSFORM_RULES = sig
  type env
  val init : fundef -> env 
  val dir : direction 
  val stmt : env -> stmt_node -> (stmt_node list) option
  val exp : env -> exp_node -> exp_node update 
  val value : env -> value_node -> value_node update 
end

module DefaultRules : functor (E : SSA_Analysis.ENV) -> TRANSFORM_RULES 

module type TRANSFORMATION = sig 
  type env
  (* return the environment used through the transformation, which can *)
  (* include any imperative changes made by the transform rules *)
  val get_env : unit -> env  
  val transform_fundef : fundef -> fundef * bool         
end

module MkTransformation : functor (R : TRANSFORM_RULES) -> TRANSFORMATION 