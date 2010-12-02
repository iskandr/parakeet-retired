open Base 

type binding = ID.t list * SSA.exp_node
type bindings = binding list 
type 'a update =
  | NoChange 
  | Update of 'a 
  | UpdateWithBindings of 'a * bindings


class default_transformation = object 
  method transform_set 
           (ids : ID.t list) 
           (rhs : SSA.exp_node) : SSA.stmt update = NoChange 
  method transform_setidx 
           (id:ID.t) 
           (indices:SSA.value_nodes) 
           (rhs:SSA.value_node ) : SSA.stmt update = NoChange
  method transform_if
           (cond:SSA.value_node) 
           (tBlock:SSA.block) 
           (fBlock:SSA.block) 
           (ifGate:SSA.if_gate) : SSA.stmt update = NoChange 
  method transform_while
           (condBlock:SSA.block)
           (condId:ID.t)
           (loopBlock:SSA.block) 
           (loopGate:SSA.loop_gate) : SSA.stmt update = NoChange 
end 

let rec transform_block f ?(stmts=DynArray.create()) = function 
  | [] -> DynArray.to_list stmts 
  | s::rest -> 
    let currStmts, currChanged = transform_stmt f s in
    List.iter (DynArray.add stmts) currStmts; 
    transform_block f ~stmts rest 
and transform_stmt f stmtNode = match stmtNode.stmt with 
  | Set (ids, rhsExp) ->
     let rhsExp',   
  | SetIdx (id, indices, rhsVal) -> 
  | If (v, tBlock, fBlock, ifGate) -> 
  | WhileLoop (condBlock, condId, bodyBlock, loopGate) -> 
        
 