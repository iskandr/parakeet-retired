open Base 
open SSA 

type binding = ID.t list * exp_node
type bindings = binding list 
type 'a update =
  | NoChange 
  | Update of 'a 
  | UpdateWithBindings of 'a * bindings

class type transformation = object 
  (* STATEMENTS *) 
  method set : ID.t list -> exp_node -> stmt update
  method setidx : ID.t -> value_nodes -> value_node -> stmt update
  method _if : value_node -> block -> block -> if_gate -> stmt update 
  method _while : block -> ID.t -> block -> loop_gate -> stmt update   

  (* EXPRESSIONS *) 
  method array_index : value_node -> value_nodes -> exp_node update 
  method array : value_nodes -> exp_node update
  method values : value_nodes -> exp_node update  

  (* EXPRESSIONS -- untyped IL only *) 
  method app : value_node -> value_nodes -> exp_node update 
    
  (* EXPRESSIONS -- typed IL only *) 
  method cast : DynType.t -> value_node -> exp_node update
  method call : typed_fn -> value_nodes -> exp_node update 
  method primapp : typed_prim -> value_nodes -> exp_node update 
  method map : closure -> value_nodes -> exp_node update 
  method reduce : closure -> closure -> value_nodes -> exp_node update 
  method scan : closure -> closure -> value_nodes -> exp_node update 

  (* VALUES *)   
  method var : ID.t -> value_node update 
  method num : PQNum.num -> value_node update 
  method str : string -> value_node update 
  method sym : string -> value_node update 
  method unit : value_node update
   
  (* VALUES -- untyped IL only *)
  method globalfn : FnId.t -> value_node update
  method prim : Prim.prim -> value_node update
  (* why is this still in the IL? *)  
  method lam : fundef -> value_node update  
end 

class default_transformation : transformation 

val transform_stmt : transformation -> stmt -> stmt update 
val transform_exp : transformation -> exp_node -> exp_node update 
val transform_value : transformation -> value_node -> value_node update
  
val transform_values : 
      transformation -> 
        ?revAcc:value_node list f -> 
        ?revBindings:binding list ->  
        ?changed:bool ->  
        value_node list ->  
        value_node list * binding list * bool 
        
val transform_block 
    : transformation -> 
        ?stmts:stmt_node DynArray.t -> 
        ?changed:bool -> 
        block -> 
        block * bool  