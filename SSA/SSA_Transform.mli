open Base 
open SSA 

type 'a update =
  | NoChange 
  | Update of 'a 
  | UpdateWithBlock of 'a * block

module type TRANSFORM_RULES = sig
  type env 
  val init : fundef -> env 
  val dir : direction 
  val stmt : env -> stmt_node -> stmt_node update 
  val exp : env -> exp_node -> exp_node update 
  val value : env -> value_node -> value_node update 
end

module DefaultRules : functor (E : SSA_Analysis.ENV) -> TRANSFORM_RULES 

module type TRANSFORMATION = sig 
  val transform_fundef : fundef -> fundef * bool     
end

module MkTransformation : functor (R : TRANSFORM_RULES) -> TRANSFORMATION 