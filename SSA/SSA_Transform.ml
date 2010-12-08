open Base 
open SSA 

type direction = Forward | Backward

type 'a update =
  | NoChange 
  | Update of 'a 
  | UpdateWithBlock of 'a * block


class type transformation = object
  method dir : direction
  
  method stmt : stmt_node -> stmt_node update 
  method exp : exp_node -> exp_node update 
  method value : value_node -> value_node update 
end

class default_transformation : transformation = object 
  method dir = Forward 
  
  method stmt _ = NoChange 
  method exp _ = NoChange
  method value _ = NoChange
end 

let mk_update data = function 
  | [] -> Update data
  | block -> UpdateWithBlock(data, block) 

let unpack_update default = function 
  | NoChange -> default, SSA.empty_block, false
  | Update other -> other, SSA.empty_block, true
  | UpdateWithBlock (other,block) -> other, block, true 


(* use a record instead of an object to track block state*)
(* for improved performance *) 
type block_state = { 
  stmts : stmt_node DynArray.t; 
  mutable changes : int;  
}     

(* initializer *) 
let fresh_block_state () = { stmts = DynArray.create (); changes = 0 }

let finalize_block_state blockState = 
  DynArray.to_array blockState.stmts, blockState.changes > 0

(* blockState methods *) 
let add_stmt blockState stmtNode = DynArray.add blockState.stmts stmtNode
let add_stmts blockState stmts = List.iter (add_stmt blockState) stmts 

let incr_changes blockState = 
  blockState.changes <- blockState.changes + 1 

(* works for both value_node and exp_node *) 
let process_update blockState xDefault update = 
  let xUpdated, stmts, changed = unpack_update xDefault update in 
  if changed then ( 
    add_stmts blockState stmts; 
    incr_changes blockState;
  ); 
  xUpdated 
    
let process_stmt_update blockState stmtNode update = 
  let stmtNode', stmts, changed = unpack_update stmtNode update in 
  if changed then (
    add_stmts blockState stmts;
    incr_changes blockState; 
  ); 
  add_stmt blockState stmtNode' 

let rec transform_block f block = 
  let blockState = fresh_block_state () in 
  let n = SSA.block_length block in
  match f#dir with 
    | Forward -> 
        for i = 0 to n - 1 do 
          blockState 
    | Backward ->  
  
  | [] -> finalize_block_state blockState  
  | s::rest -> 
    (* transform_stmt returns unit since its potential outputs *)
    (* are captured by blockState *)  
    transform_stmt blockState f s; 
    transform_block ~blockState f rest  
    
and transform_stmt blockState f stmtNode = 
  let oldNumChanges = blockState.changes in 
  let stmtNode' = match stmtNode.stmt with 
    | Set (ids, rhsExpNode) ->
      let rhsExpNode' = transform_exp blockState f rhsExpNode in  
      if oldNumChanges <> blockState.changes then 
        {stmtNode with stmt=Set(ids, rhsExpNode') }
      else stmtNode 
     
    | SetIdx (id, indices, rhsVal) ->
        let indices' = transform_values blockState f indices in
        let rhsVal' =  transform_value blockState f rhsVal in
        if oldNumChanges <> blockState.changes  then 
          {stmtNode with stmt=SetIdx(id, indices', rhsVal')} 
        else stmtNode 
    | If (v, tBlock, fBlock, ifGate) -> 
        failwith "if not implemented" 
    | WhileLoop (condBlock, condId, bodyBlock, loopGate) -> 
        failwith "loop not implemented" 
  in
  process_stmt_update blockState stmtNode' (f#stmt stmtNode')
     
and transform_exp blockState f expNode = 
  let oldNumChanges = blockState.changes in 
  let expNode' = match expNode.exp with 
    | Values vs ->
      let vs' = transform_values blockState f vs in
      if blockState.changes <> oldNumChanges then 
        {expNode with exp = Values vs'}
      else expNode 
    | App(fn,args) ->
      let fn'= transform_value blockState f fn in 
      let args' = transform_values blockState f args in
      if blockState.changes <> oldNumChanges then 
        {expNode with exp = App(fn', args') } 
      else expNode
  in 
  process_update blockState expNode' (f#exp expNode')    

and transform_values blockState f ?(revAcc=[])  = function  
  | [] -> List.rev revAcc 
  | v::vs ->
      let oldNumChanges = blockState.changes in 
      let v' = transform_value blockState f v in 
      transform_values
        blockState  
        f 
        ~revAcc:(v' :: revAcc) 
        vs
          
and transform_value blockState f vNode = 
  process_update blockState vNode (f#value vNode)

let transform_fundef f fundef = 
  let body', changed = transform_block f fundef.body in
  {fundef with body = body'}, changed
