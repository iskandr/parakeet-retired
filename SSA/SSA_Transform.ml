open Base 
open SSA 


type binding = ID.t list * exp_node
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

class default_transformation : transformation = object 
  (* STATEMENTS *) 
  method set _ _ = NoChange
  method setidx _ _ _ = NoChange
  method _if _ _ _ _  = NoChange 
  method _while _ _ _ _= NoChange    

  (* EXPRESSIONS *) 
  method array_index _ _  = NoChange 
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

let rec bindings_to_stmts src = function 
  | [] -> []
  | (ids, expNode)::bs ->
      let stmtNode = SSA.mk_set ?src ids expNode in 
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
  let (exp', types'), bindings, changed = 
    unpack_update (expNode.exp, expNode.exp_types) update 
  in 
  if changed then
    let expNode' = {expNode with exp = exp'; exp_types=types'} in
    let stmts = bindings_to_stmts expNode.exp_src bindings in   
    expNode', stmts, true 
  else
    expNode, [], false

let unpack_value_update valNode update : value_node * block * bool = 
  let (val', t'), bindings, changed = 
    unpack_update (valNode.value, valNode.value_type) update 
  in 
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
  

(* use a record instead of an object to track block state*)
(* for improved performance *) 
type block_state = { 
  stmts : stmt_node DynArray.t; 
  mutable changes : int;  
}     

(* initializer *) 
let fresh_block_state () = { stmts = DynArray.create (); changes = 0 }

let finalize_block_state blockState = 
  DynArray.to_list blockState.stmts, blockState.changes > 0

(* blockState methods *) 
let add_stmt blockState stmtNode = DynArray.add blockState.stmts stmtNode
let add_stmts blockState stmts = List.iter (add_stmt blockState) stmts 

let incr_changes blockState = 
  blockState.changes <- blockState.changes + 1 
  
  (*
let update_flag blockState changed = 
  blockState.changed <- blockState.changed || changed
    *)
    
let process_value_update blockState defaultValueNode vUpdate = 
  let v', (stmts:block), changed = 
    unpack_value_update defaultValueNode vUpdate 
  in
  add_stmts blockState stmts;
  if changed then incr_changes blockState;  
  v'
   
            
let process_exp_update blockState expNode update = 
  let expNode', stmts, changed = unpack_exp_update expNode update in 
  add_stmts blockState stmts;
  if changed then incr_changes blockState;   
  expNode'    
 
let process_stmt_update blockState stmtNode update = 
  let stmts, changed = unpack_stmt_update stmtNode update in 
  add_stmts blockState stmts;
  if changed then incr_changes blockState

let rec transform_block ?(blockState = fresh_block_state ()) f = function 
  | [] -> finalize_block_state blockState  
  | s::rest -> 
    (* transform_stmt returns unit since its potential outputs *)
    (* are captured by blockState *)  
    transform_stmt blockState f s; 
    transform_block ~blockState f rest  
    
and transform_stmt blockState f stmtNode = 
  let oldNumChanges = blockState.changes in 
  match stmtNode.stmt with 
  | Set (ids, rhsExpNode) ->
    let rhsExpNode' = transform_exp blockState f rhsExpNode in  
    let stmtNode' = 
      if oldNumChanges <> blockState.changes then 
        {stmtNode with stmt=Set(ids, rhsExpNode') }
      else stmtNode 
    in 
    process_stmt_update blockState stmtNode' (f#set ids rhsExpNode')
     
  | SetIdx (id, indices, rhsVal) ->
    let indices' = transform_values blockState f indices in
    let rhsVal' =  transform_value blockState f rhsVal in
    let stmtNode' = 
      if oldNumChanges <> blockState.changes  then 
        {stmtNode with stmt=SetIdx(id, indices', rhsVal')} 
      else stmtNode 
    in     
    process_stmt_update blockState stmtNode' (f#setidx id indices' rhsVal')
  | If (v, tBlock, fBlock, ifGate) -> 
      failwith "if not implemented" 
  | WhileLoop (condBlock, condId, bodyBlock, loopGate) -> 
      failwith "loop not implemented" 
      
and transform_exp blockState f expNode = 
  let oldNumChanges = blockState.changes in 
  match expNode.exp with 
  | Values vs ->
      let vs' = transform_values blockState f vs in
      let expNode' = 
        if blockState.changes <> oldNumChanges then 
          {expNode with exp = Values vs'}
        else expNode 
      in 
      process_exp_update blockState expNode' (f#values vs')
      
  | App(fn,args) ->
      let fn'= transform_value blockState f fn in 
      let args' = transform_values blockState f args in
      let expNode' = 
        if blockState.changes <> oldNumChanges then 
          {expNode with exp = App(fn', args') } 
        else expNode
      in
      process_exp_update blockState expNode' (f#app fn' args')  
      
and transform_values blockState f ?(revAcc=[])  = function  
  | [] -> List.rev revAcc 
  | v::vs ->
      let oldNumChanges = blockState.changes in 
      let v' = transform_value blockState f v in 
      let revAcc' = 
        if oldNumChanges <> blockState.changes then 
          v' :: revAcc 
        else 
          v :: revAcc 
      in 
      transform_values
        blockState  
        f 
        ~revAcc:revAcc' 
        vs
          
 and transform_value blockState f vNode = 
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
  process_value_update blockState vNode update

let transform_fundef f fundef = 
  let body', changed = transform_block f fundef.body in
  {fundef with body = body'}, changed
