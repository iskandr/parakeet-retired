open Base 
open SSA 

type direction = Forward | Backward

type 'a update =
  | NoChange 
  | Update of 'a 
  | UpdateWithStmts of 'a * (stmt_node list)
  | UpdateWithBlock of 'a * block


(* used for top-down transformations *) 
type val_helper = (value_node -> value_node update) -> value_node -> value_node
type exp_helper = (exp_node -> exp_node update) -> exp_node -> exp_node   

type 'a custom_traversal = 
  'a -> exp_helper -> val_helper -> stmt_node -> (stmt_node list) option

module type TRANSFORM_RULES = sig
  val dir : direction
  type context
  val init : fundef -> context  
  val finalize : context -> fundef -> fundef update
  
  val stmt : env -> stmt_node -> (stmt_node list) option  
  val exp : env -> exp_node -> exp_node update 
  val value : env -> value_node -> value_node update 
  
  val custom_traversal : (context custom_traversal) option 
end

module DefaultRules (E : SSA_Analysis.ENV) = struct
  type env = E.t
  let init = E.init 
  let dir = Forward
  let fundef _ _ = NoChange
  let stmt _ _ = None 
  let exp _ _ = NoChange
  let value _ _ = NoChange 
  let custom_traversal = None 
end

module BlockState = struct 
  (* use a record instead of an object to track block state*)
  (* for improved performance *) 
  type t = { 
    stmts : stmt_node DynArray.t; 
    mutable changes : int;  
  }     

  (* initializer *) 
  let create () = { stmts = DynArray.create (); changes = 0 }

  let finalize blockState = 
    DynArray.to_array blockState.stmts, blockState.changes > 0

  (* blockState methods *) 
  let add_stmt blockState stmtNode = DynArray.add blockState.stmts stmtNode
  let add_stmt_list blockState stmts = 
    List.iter (add_stmt blockState) stmts 
  
  let add_block blockState block = 
    block_iter_forward (add_stmt blockState) block 
  
  let incr_changes blockState = 
    blockState.changes <- blockState.changes + 1 

  (* works for both value_node and exp_node *) 
  let process_update blockState xDefault = function 
    | NoChange -> xDefault
    | Update xNew -> incr_changes blockState; xNew 
    | UpdateWithStmts (xNew, stmts) -> 
        incr_changes blockState; 
        add_stmt_list blockState stmts; 
        xNew  
    | UpdateWithBlock (xNew, block) -> 
        incr_changes blockState; 
        add_block blockState block; 
        xNew 
   
  let process_stmt_update blockState stmtNode update =
    match update with 
      | None -> add_stmt blockState stmtNode 
      | Some stmts -> 
          add_stmt_list blockState stmts;
          incr_changes blockState  
end 
open BlockState 

module type TRANSFORMATION = sig 
  val transform_fundef : fundef -> fundef * bool     
end
module MkTransformation(R : TRANSFORM_RULES) = struct 
  type context = R.context 
   
  let rec transform_block cxt block = 
    let blockState = BlockState.create() in  
    let n = SSA.block_length block in
    (match R.dir with 
    | Forward -> 
        for i = 0 to n - 1 do 
          transform_stmt blockState cxt (block_idx block i)
        done 
    | Backward ->  
        for i = n-1 downto 0 do 
          transform_stmt blockState cxt (block_idx block i)
        done  
    );
    BlockState.finalize blockState 
     
  and transform_stmt blockState cxt stmtNode = 
    let oldNumChanges = blockState.changes in
    
    let stmtNode' = match stmtNode.stmt with 
    | Set (ids, rhsExpNode) ->
      let rhsExpNode' = transform_exp blockState cxt rhsExpNode in  
      if oldNumChanges <> blockState.changes then 
        {stmtNode with stmt=Set(ids, rhsExpNode') }
      else stmtNode 
     
    | SetIdx (id, indices, rhsVal) ->
        let indices' = transform_values blockState cxt indices in
        let rhsVal' =  transform_value blockState cxt rhsVal in
        if oldNumChanges <> blockState.changes  then 
          {stmtNode with stmt=SetIdx(id, indices', rhsVal')} 
        else stmtNode 
    | If (v, tBlock, fBlock, ifGate) -> 
        failwith "if not implemented" 
    | WhileLoop (condBlock, condId, bodyBlock, loopGate) -> 
        failwith "loop not implemented" 
  in
  BlockState.process_stmt_update blockState stmtNode' (R.stmt env stmtNode')
     
  and transform_exp blockState cxt expNode = 
    let oldNumChanges = blockState.changes in 
    let expNode' = match expNode.exp with 
    | Values vs ->
      let vs' = transform_values blockState cxt vs in
      if blockState.changes <> oldNumChanges then 
        {expNode with exp = Values vs'}
      else expNode 
    | App(fn,args) ->
      let fn'= transform_value blockState env fn in 
      let args' = transform_values blockState cxt args in
      if blockState.changes <> oldNumChanges then 
        {expNode with exp = App(fn', args') } 
      else expNode
  in 
  BlockState.process_update blockState expNode' (R.exp env expNode')    

  and transform_values blockState cxt ?(revAcc=[])  = function  
  | [] -> List.rev revAcc 
  | v::vs ->
      let oldNumChanges = blockState.changes in 
      let v' = transform_value blockState cxt v in 
      transform_values
        blockState  
        cxt 
        ~revAcc:(v' :: revAcc) 
        vs
          
  and transform_value blockState cxt vNode = 
    BlockState.process_update blockState vNode (R.value cxt vNode)

  (* these exist so we can access the transform env from outside functions, *)
  (* in case it contains useful information *) 
  let globalContext = ref None 
  let set_context c = globalContext := Some c
  let get_context () = match !globalContext with 
    | None -> assert false 
    | Some c-> c 
   
  (* 1) initialize the generic environment type 
     2) run transformations over all statements in the body 
     3) run a final transformation on the full function definition 
  *) 
  let transform_fundef fundef =
    let cxt = R.init fundef in
    set_context cxt;     
    let body', changed = transform_block cxt fundef.body in
    let fundef' = {fundef with body = body'} in  
    match R.finalize cxt fundef' with 
      | NoChange -> fundef', changed
      | Update fundef'' -> fundef'', true 
      | UpdateWithStmts (fundef'', stmts) -> 
        { fundef'' with body = block_append (block_of_list stmts) body'}, true   
          
      | UpdateWithBlock (fundef'', block) ->
        { fundef'' with body = block_append block body' }, true     
end 

module DefaultTransformation (E: SSA_Analysis.ENV) = 
  MkTransformation(DefaultRules(E))
