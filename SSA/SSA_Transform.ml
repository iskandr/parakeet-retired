open Base 
open SSA 

type binding = ID.t list * SSA.exp_node
type bindings = binding list 
type 'a update =
  | NoChange 
  | Update of 'a 
  | UpdateWithBindings of 'a * bindings

type sUpate = stmt update 
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

let bindings_to_stmts src = function 
  | [] -> []
  | (ids, expNode)::bs ->
      let stmtNode = SSA.mk_set ~src ids expNode in 
      stmtNode :: (bindings_to_stmts src bs) 

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

let unpack_stmt_update stmtNode update : block * bool =  
  let stmt', bindings, changed = unpack_update stmtNode.stmt update in 
  if changed then
    let stmtNode' = {stmtNode with stmt = stmt'} in  
    let stmts = (bindings_to_stmts stmtNode.stmt_src bindings) @ [stmtNode'] in
    stmts, true 
  else 
    [stmtNode], false 

let unpack_exp_update expNode update : exp_node * block * bool = 
  let (exp', types'), bindings, changed = unpack_update expNode.exp update in 
  if changed then
    let expNode' = {expNode with exp = exp'; exp_types=types'} in
    let stmts = bindings_to_stmts expNode.exp_src bindings in   
    expNode', stmts, true 
  else
    expNode, [], false

let unpack_value_update valNode update : value_node * block * bool = 
  let (val', t'), bindings, changed = unpack_update valNode.value update in 
  if changed then 
    let stmts = bindings_to_stmts valNode.value_src bindings in 
    let valNode' = {valNode with value = val'; value_type = t'} in 
    valNode', stmts, true 
  else 
    valNode, [], false 

let unpacked_zip (d1, bs1, c1) (d2, bs2, c2) = 
    (d1,d2), bs1 @ bs2, c1 || c2
    
let unpacked_zip3 (d1, bs1, c1) (d2, bs2, c2) (d3, bs3, c3) = 
    (d1, d2, d3), bs1 @ bs2 @ bs3, c1 || c2 || c3 

let unpacked_zip4 (d1, bs1, c1) (d2, bs2, c2) (d3, bs3, c3) (d4, bs4, c4) = 
    (d1, d2, d3, d4), bs1 @ bs2 @ bs3 @ bs4, c1 || c2 || c3 || c4         
  

let rec transform_block f ?(stmts=DynArray.create()) ?(changed=false) = function 
  | [] -> DynArray.to_list stmts, changed 
  | s::rest -> 
    let currStmts, currChanged = transform_stmt f s in
    List.iter (DynArray.add stmts) currStmts; 
    transform_block f ~stmts ~changed:(changed || currChanged) rest 
    
and transform_stmt f stmtNode = 
  match stmtNode.stmt with 
  | Set (ids, rhsExpNode) ->
    let rhsExpNode', rhsStmts, rhsChanged =
      unpack_exp_update rhsExpNode (transform_exp f rhsExpNode) 
    in 
    let stmtNode' = 
      if rhsChanged then 
        {stmtNode with stmt=Set(ids, rhsExpNode') }
      else stmtNode 
    in 
    let mainStmts = unpack_stmt_update stmtNode' (f#set ids rhsExpNode') in 
    rhsStmts @ mainStmts
     
  | SetIdx (id, indices, rhsVal) ->
    let indices', indexStmts, indicesChanged = transform_values f indices in
    let rhsVal', rhsStmts, rhsChanged =  transform_value f rhsVal in
    let stmtNode' = 
      if rhsChanged || indicesChanged then 
        {stmtNode with stmt=SetIdx(id, indices', rhsVal')} 
      else stmtNode 
    in     
    let mainStmts = 
      unpack_stmt_update stmtNode' (f#setidx id indices' rhsVal')
    in 
    rhsStmts @ indexStmts @ mainStmts
               
  | If (v, tBlock, fBlock, ifGate) -> 
      failwith "if not implemented" 
  | WhileLoop (condBlock, condId, bodyBlock, loopGate) -> 
      failwith "loop not implemented"
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
              
      
      
and transform_values f ?(revAcc=[]) ?(revStmts=[]) ?(changed=false) = 
  function 
  | [] -> List.rev revAcc, List.rev revStmts, changed
  | v::vs ->
      let v', stmts, currChanged = transform_value f v in 
      if currChanged then
        transform_values 
          f 
          ~revAcc:(v' :: revAcc)
          ~revStmts:( List.rev_append stmts revStmts)
          ~changed:true 
          vs
      else 
        transform_values 
          f 
          ~revAcc:(v::revAcc)
          ~revStmts
          ~changed 
          vs
          
 and transform_value f vNode = 
  let update = match vNode.value with 
    | Num n -> f#num n 
    | Var id -> f#var id 
    | GlobalFn fnId -> f#globalfn fnId   
    | Str s -> f#str s 
    | Sym s -> f#sym s
    | Unit -> f#unit
    | Prim p -> f#prim p
    | Lam fundef -> f#lam fundef 
  in 
  unpack_value_update vNode update
  

let transform_fundef f fundef = 
  let body', changed = transform_block f fundef.body in
  if changed then {fundef with body = body'}, true
  else fundef, false 
