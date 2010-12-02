open Base 
open SSA 

type binding = ID.t list * SSA.exp_node
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

class default_transformation : transformation = object 
  (* STATEMENTS *) 
  method set _ _ = NoChange
  method setidx _ _ _ = NoChange
  method _if _ _ _ _  = NoChange 
  method _while _ _ _ _= NoChange    

  (* EXPRESSIONS *) 
  method array_index _ _ _ = NoChange 
  method array _ = NoChange
  method values _ = NoChange  

  (* EXPRESSIONS -- untyped IL only *) 
  method app _ _ = NoChange 
    
  (* EXPRESSIONS -- typed IL only *) 
  method cast _ _ = NoChange
  method call _ _ = NoChange 
  method primapp _ _ = NoChange  
  method map _ _ = NoChange 
  method reduce _ _ _ = NoChange 
  method scan _ _ _ = NoChange 

  (* VALUES *)   
  method var _ = NoChange 
  method num _ = NoChange 
  method str _ = NoChange 
  method sym _ = NoChange 
  method unit = NoChange
   
  (* VALUES -- untyped IL only *)
  method globalfn _ = NoChange
  method prim _ = NoChange
  (* why is this still in the IL? *)  
  method lam  _  = NoChange 
end

let prepend_bindings update bindings = match update with 
  | NoChange -> failwith "expected update"
  | Update data -> 
      if bindings = [] then update 
      else UpdateWithBindings(data, bindings)
  | UpdateWithBindings(data, moreBindings) -> 
      UpdateWithBindings(data, bindings@moreBindings)

let mk_update data = function 
  | [] -> Update data
  | bindings -> UpdateWithBindings(data, bindings) 

let unpack_update default = function 
  | NoChange -> default, [], false
  | Update other -> other, [], true
  | UpdateWithBindings (other,bs) -> other, bs, true 

let unpacked_zip (d1, bs1, c1) (d2, bs2, c2) = 
    (d1,d2), bs1 @ bs2, c1 || c2
    
let unpacked_zip3 (d1, bs1, c1) (d2, bs2, c2) (d3, bs3, c3) = 
    (d1, d2, d3), bs1 @ bs2 @ bs3, c1 || c2 || c3 

let unpacked_zip4 (d1, bs1, c1) (d2, bs2, c2) (d3, bs3, c3) (d4, bs4, c4) = 
    (d1, d2, d3, d4), bs1 @ bs2 @ bs3 @ bs4, c1 || c2 || c3 || c4         
  

let rec transform_block f ?(stmts=DynArray.create()) = function 
  | [] -> DynArray.to_list stmts 
  | s::rest -> 
    let currStmts, currChanged = transform_stmt f s in
    List.iter (DynArray.add stmts) currStmts; 
    transform_block f ~stmts rest 
and transform_stmt f stmtNode = match stmtNode.stmt with 
  | Set (ids, rhsExp) ->
       transform_exp rhsExp    
  | SetIdx (id, indices, rhsVal) -> 
  | If (v, tBlock, fBlock, ifGate) -> 
  | WhileLoop (condBlock, condId, bodyBlock, loopGate) ->
and transform_exp f expNode = match expNode.exp with 
  | Values vs -> 
      let vs', bindings, childrenChanged = transform_values f vs in 
      (match f#values vs' with 
        | NoChange -> 
            if childrenChanged then 
              let expNode' = { expNode with exp = Values vs' }
              mk_update expNode' bindings 
            else NoChange  
        | update -> prepend_bindings update bindings 
       )    
  | App(fn,args) ->
      let args', argBindings, argsChanged = transform_values f args in 
      let fn', fnBindings, fnChanged = unpack fn (transform_value f fn) in
      let bindings = fnBindings @ argBindings in
      let childrenChanged = argsChanged || fnChanged in
      (match f#app fn' args' with 
        | NoChange -> 
            if childrenChanged then 
              let expNode' = { expNode with exp = App(fn', args') } in 
              mk_update expNode' bindings   
            else NoChange  
       | update -> prepend_bindings update bindings
      ) 
              
      
      
and transform_values f ?(revAcc=[]) ?(revBindings=[]) ?(changed=false) = 
  function 
  | [] -> List.rev revAcc, List.rev revBindings, changed
  | v::vs -> 
      begin match transform_value f v with 
        | NoChange ->
            transform_values 
              f 
              ~revAcc:(v::revAcc)
              ~revBindings 
              ~changed 
              vs
        | Update v' -> 
            transform_values 
              f 
              ~revAcc:(v'::revAcc)
              ~revBindings
              ~changed:true 
              vs
        | UpdateWithBindings (v', bs) ->
            transform_values 
              f 
              ~revAcc:(v'::revAcc)
              ~revBindings:(List.rev_append bs revBindings) 
              ~changed:true 
              vs
      end 
         
 