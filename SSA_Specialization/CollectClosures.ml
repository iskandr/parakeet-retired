open SSA
open SSA_Transform
 
module Collector = struct 
  type env = { 
    untyped_closures : (ID.t, value) Hashtbl.t; 
    untyped_closure_args : (ID.t, ID.t list) Hashtbl.t 
  } 
   
  let init _ = { 
    untyped_closures = Hashtbl.create 127; 
    untyped_closure_args = Hashtbl.create 127
  }  
  let dir = Forward 
   
  let stmt : env -> stmt_node -> stmt_node update 
  val exp : env -> exp_node -> exp_node update 
  val value : env -> value_node -> value_node update 
  
end 