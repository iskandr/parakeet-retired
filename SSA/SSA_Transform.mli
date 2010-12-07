open Base 
open SSA 


type binding = ID.t list * SSA.exp_node
type bindings = binding list 
type 'a update =
  | NoChange 
  | Update of 'a 
  | UpdateWithBindings of 'a * bindings

class type transformation = object 
  method stmt : stmt_node -> stmt_node update 
  method exp : exp_node -> exp_node update 
  method value : value_node -> value_node update 
end

class default_transformation : transformation 

val bindings_to_stmts 
    : SourceInfo.source_info option -> bindings -> stmt_node list 
val mk_update : 'a -> bindings -> 'a update 

val unpack_update : 'a -> 'a update -> 'a * bindings * bool 

type block_state = { 
  stmts : stmt_node DynArray.t; 
  mutable changes : int; 
}     
val fresh_block_state : unit -> block_state 
val add_stmt : block_state -> stmt_node -> unit

(* add statements to block state, return any exp/value and a change flag *)  
val process_update : block_state -> 'a -> 'a update -> 'a  
val process_stmt_update : block_state -> stmt_node -> stmt_node update -> unit 

val transform_stmt : block_state -> transformation -> stmt_node -> unit  
val transform_exp 
    : block_state -> transformation -> exp_node -> exp_node      
val transform_value 
    : block_state -> transformation -> value_node -> value_node  
  
val transform_values : 
      block_state -> 
        transformation -> 
        ?revAcc:(value_node list) ->  
        value_node list -> 
        value_node list 
        
val transform_block : 
      ?blockState:(block_state) -> 
        transformation -> 
        block -> 
        block * bool  

val transform_fundef : transformation -> fundef -> fundef * bool 
        