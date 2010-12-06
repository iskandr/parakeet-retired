open Base 
open SSA 


type binding = ID.t list * SSA.exp_node
type bindings = binding list 
type 'a update =
  | NoChange 
  | Update of 'a 
  | UpdateWithBindings of 'a * bindings

type sUpdate = stmt update 
type eUpdate = (exp * DynType.t list) update
type vUpdate = (value * DynType.t) update   


class type transformation = object 
   
  (* STATEMENTS *) 
  method set : ID.t list -> exp_node -> sUpdate
  method setidx : ID.t -> value_nodes -> value_node -> sUpdate
  method _if : value_node -> block -> block -> if_gate -> sUpdate 
  method _while : block -> ID.t -> block -> loop_gate -> sUpdate   
  

  (* EXPRESSIONS *) 
  method array_index : value_node -> value_nodes -> eUpdate 
  method array : value_nodes -> eUpdate
  method values : value_nodes -> eUpdate  

  (* EXPRESSIONS -- untyped IL only *) 
  method app : value_node -> value_nodes -> eUpdate 
    
  (* EXPRESSIONS -- typed IL only *) 
  method cast : DynType.t -> value_node -> eUpdate
  method call : typed_fn -> value_nodes -> eUpdate 
  method primapp : typed_prim -> value_nodes -> eUpdate 
  method map : closure -> value_nodes -> eUpdate 
  method reduce : closure -> closure -> value_nodes -> eUpdate 
  method scan : closure -> closure -> value_nodes -> eUpdate 

  (* VALUES *)   
  method var : ID.t -> vUpdate 
  method num : PQNum.num -> vUpdate 
  method str : string -> vUpdate 
  method sym : string -> vUpdate 
  method unit : vUpdate
   
  (* VALUES -- untyped IL only *)
  method globalfn : FnId.t -> vUpdate
  method prim : Prim.prim -> vUpdate
  (* why is this still in the IL? *)  
  method lam : fundef -> vUpdate  
end 

class default_transformation : transformation 

val bindings_to_stmts 
    : SourceInfo.source_info option -> bindings -> stmt_node list 
val prepend_bindings : 'a update -> bindings -> 'a update 
val mk_update : 'a -> bindings -> 'a update 
val unpack_update : 'a -> 'a update -> 'a * bindings * bool 
val unpack_stmt_update : stmt_node -> sUpdate -> block * bool 
val unpack_exp_update : exp_node -> eUpdate -> exp_node * block * bool 
val unpack_value_update : value_node -> vUpdate -> value_node * block * bool 


type block_state = { 
  stmts : stmt_node DynArray.t; 
  mutable changes : int; 
}     
val fresh_block_state : unit -> block_state 
val add_stmt : block_state -> stmt_node -> unit

(* add statements to block state, return any exp/value and a change flag *)  
val process_value_update 
    : block_state -> value_node -> vUpdate -> value_node  
val process_exp_update 
    : block_state -> exp_node -> eUpdate -> exp_node   
val process_stmt_update : block_state -> stmt_node -> sUpdate -> unit 

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
        