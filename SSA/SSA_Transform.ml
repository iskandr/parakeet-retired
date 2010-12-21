open Base 
open SSA 

type direction = Forward | Backward

type 'a update =
  | NoChange 
  | Update of 'a 
  | UpdateWithStmts of 'a * (stmt_node list)
  | UpdateWithBlock of 'a * block

module type SIMPLE_TRANSFORM_RULES = sig
  val dir : direction
  type context
  val init : fundef -> context  
  val finalize : context -> fundef -> fundef update
  val stmt : context -> stmt_node -> stmt_node update  
  val exp : context -> exp_node -> exp_node update 
  val value : context -> value_node -> value_node update 
end

(* used for custom top-down transformations *) 
type rewrite_helpers = { 
  version : unit -> int; 
  changed : int -> bool; 
  process_value:  (value_node -> value_node update) -> value_node -> value_node; 
  process_exp: (exp_node -> exp_node update) -> exp_node -> exp_node;
  process_block: 
    (rewrite_helpers -> stmt_node -> stmt_node update) -> block -> block;
}  

module type CUSTOM_TRANSFORM_RULES = sig
  val dir : direction 
  type context 
  val init : fundef -> context 
  val finalize : context -> fundef -> fundef update
  val stmt : context -> rewrite_helpers -> stmt_node -> stmt_node update  
end

module BlockState = struct 
  (* use a record instead of an object to track block state*)
  (* for improved performance *) 
  type t = { 
    stmts : stmt_node DynArray.t; 
    mutable changes : int;  
  }     

  (* initializer *) 
  let create () = { 
    stmts = DynArray.create (); 
    changes = 0 
  }

  let finalize blockState = 
    DynArray.to_array blockState.stmts, blockState.changes > 0

  (* add statement to block unless it's a no-op *)  
  let add_stmt blockState stmtNode = 
    if not (SSA.is_empty_stmt stmtNode) then 
      DynArray.add blockState.stmts stmtNode
      
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
    let stmtNode' = process_update blockState stmtNode update in 
    add_stmt blockState stmtNode'   
end 
open BlockState 

module MkCustomTransform(R : CUSTOM_TRANSFORM_RULES) = struct 
  let rec transform_block 
          (rewriteStmt : rewrite_helpers -> stmt_node -> stmt_node update) 
          (block:block) = 
    let blockState = BlockState.create () in
    let rec helpers = { 
      version = (fun() -> blockState.changes); 
      changed = (fun n -> blockState.changes <> n); 
      process_value = 
        (fun f vNode -> BlockState.process_update blockState vNode (f vNode));
      process_exp = 
        (fun f eNode -> BlockState.process_update blockState eNode (f eNode));
      (* recursion in action! *)
      process_block = 
        (fun rewriteStmt' block' -> 
         let newBlock, _ = transform_block rewriteStmt' block' in newBlock)
      (*    (rewrite_helpers -> stmt_node -> stmt_node update) -> block -> block;*)
    }
    in  
    let n = SSA.block_length block in
    (match R.dir with 
    | Forward -> 
        for i = 0 to n - 1 do
          let stmtNode = block_idx block i  in 
          let stmtUpdate = rewriteStmt helpers stmtNode in
          BlockState.process_stmt_update blockState stmtNode stmtUpdate
        done 
    | Backward ->  
        for i = n-1 downto 0 do 
          let stmtNode = block_idx block i  in 
          let stmtUpdate = rewriteStmt helpers stmtNode in
          BlockState.process_stmt_update blockState stmtNode stmtUpdate
        done  
    );
    BlockState.finalize blockState 
    
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
    let body', changed = transform_block (R.stmt cxt) fundef.body in
    let fundef' = {fundef with body = body'} in  
    match R.finalize cxt fundef' with 
      | NoChange -> fundef', changed
      | Update fundef'' -> fundef'', true 
      | UpdateWithStmts (fundef'', stmts) -> 
        { fundef'' with body = block_append (block_of_list stmts) body'}, true   
      | UpdateWithBlock (fundef'', block) ->
        { fundef'' with body = block_append block body' }, true     
end 

module CustomFromSimple(R: SIMPLE_TRANSFORM_RULES) = struct
  let dir = R.dir 
  
  type context = R.context
  let init = R.init
  let finalize = R.finalize 
  
  let transform_value helpers cxt vNode = 
    helpers.process_value (R.value cxt) vNode
     
  let transform_values helpers cxt vNodes = 
    List.map (transform_value helpers cxt) vNodes   
  
  let transform_exp helpers cxt expNode =
    let oldV = helpers.version () in
    let changed () = helpers.version () <> oldV in  
    let expNode' = match expNode.exp with 
    | Values vs -> 
      let vs' = transform_values helpers cxt vs in
      if changed() then {expNode with exp = Values vs'} else expNode  
    | App(fn,args) -> 
      let fn' = transform_value helpers cxt fn in 
      let args' = transform_values helpers cxt args in 
      if changed () then {expNode with exp = App(fn', args')} 
      else expNode 
    | _ -> failwith "not implemented"  
    in 
    helpers.process_exp (R.exp cxt) expNode'  
           
  let rec stmt cxt helpers stmtNode =
    let oldV = helpers.version () in
    let changed () = helpers.version () <> oldV in  
    let stmtNode' = match stmtNode.stmt with 
    | Set (ids, rhsExpNode) ->
      let rhsExpNode' = transform_exp helpers cxt rhsExpNode in  
      if changed() then {stmtNode with stmt=Set(ids, rhsExpNode') }
      else stmtNode 
     
    | SetIdx (id, indices, rhsVal) ->
      let indices' = transform_values helpers cxt indices in
      let rhsVal' =  transform_value helpers cxt rhsVal in
      if changed() then 
        {stmtNode with stmt=SetIdx(id, indices', rhsVal')} 
      else stmtNode 
    | If (v, tBlock, fBlock, ifGate) -> 
      let v' = transform_value helpers cxt v in 
      let tBlock' = helpers.process_block (stmt cxt) tBlock in 
      let fBlock' = helpers.process_block (stmt cxt) fBlock in 
      if changed() then 
        { stmtNode with stmt = If(v', tBlock', fBlock', ifGate) }
      else stmtNode  
    | WhileLoop (condBlock, condVal, bodyBlock, loopGate) ->
      let condBlock' = helpers.process_block (stmt cxt) condBlock in
      let condVal' = transform_value helpers cxt condVal in  
      let bodyBlock' = helpers.process_block (stmt cxt) bodyBlock in 
      if changed() then 
        { stmtNode with stmt=WhileLoop(condBlock',condVal',bodyBlock',loopGate)}
      else stmtNode 
    in
    R.stmt cxt stmtNode'  
end 

module MkSimpleTransform(R : SIMPLE_TRANSFORM_RULES) = 
  MkCustomTransform(CustomFromSimple(R)) 