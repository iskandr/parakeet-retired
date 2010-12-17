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
  
  val stmt : env -> stmt_node -> (stmt_node list) option  
  val exp : env -> exp_node -> exp_node update 
  val value : env -> value_node -> value_node update 
end

(* used for custom top-down transformations *) 
type rewrite_helpers = { 
  get_version : unit -> int; 
  changed : int -> bool; 
  val_helper:  (value_node -> value_node update) -> value_node -> value_node; 
  exp_helper: (exp_node -> exp_node update) -> exp_node -> exp_node;
}  

module type CUSTOM_TRANSFORM_RULES = sig
  val dir : direction 
  type context 
  val init : fundef -> context 
  val finalize : context -> fundef -> fundef update
  
  val stmt : context -> rewrite_helpers -> stmt_node -> (stmt_node list) option  
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

module MkCustomTransform(R : CUSTOM_TRANSFORM_RULES) = struct 
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
    let helpers = { 
      get_version = (fun() -> blockState.changes); 
      changed = (fun n -> BlockState.changes <> n); 
      val_helper = 
        (fun f node -> BlockState.process_update blockState node (f node));
      exp_helper = 
        (fun f node -> BlockState.process_update blockState node (f node));
    }
    in 
    let stmtUpdate = R.stmt cxt helpers stmtNode in 
    BlockState.process_stmt_update blockState stmtNode stmtUpdate 
    
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

module CustomFromSimple(R: SIMPLE_TRANSFORM_RULES) = struct
  let dir = R.dir 
  
  type context = R.context
  let init = R.init
  let finalize = R.finalize 
  
  let transform_value helpers cxt vNode = 
    helpers.value_helper (R.value cxt) vNode
     
  let transform_values vNodes = List.map transform_value vNodes   
  
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
    helpers.exp_helper (R.exp cxt) expNode'  
           
  let stmt cxt helpers stmtNode =
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
        if changed()  then 
          {stmtNode with stmt=SetIdx(id, indices', rhsVal')} 
        else stmtNode 
    | If (v, tBlock, fBlock, ifGate) -> 
        failwith "if not implemented" 
    | WhileLoop (condBlock, condId, bodyBlock, loopGate) -> 
        failwith "loop not implemented"
    in 
    BlockState.process_stmt_update (R.stmt cxt stmtNode')
end 

module MkSimpleTransform(R : SIMPLE_TRANSFORM_RULES) = 
  MkCustomTransform(CustomFromSimple(R)) 