open Base
open PhiNode
open TypedSSA

type direction = Forward | Backward

type 'a update =
  | NoChange
  | Update of 'a
  | UpdateWithStmts of 'a * (stmt_node list)
  | UpdateWithBlock of 'a * block

module type SIMPLE_TRANSFORM_RULES = sig
  val dir : direction
  type context
  val init : fn -> context
  val finalize : context -> fn -> fn update
  val stmt : context -> stmt_node -> stmt_node update
  val exp : context -> exp_node -> exp_node update
  val value : context -> value_node -> value_node update
  val phi : context -> phi_node -> phi_node update
end

module BlockState = struct
  (* use a record instead of an object to track block state*)
  (* for improved performance *)
  type t = {
    stmts : (stmt_node list) ref;
    old_block : TypedSSA.block;
    mutable changes : int;
  }

  (* initializer *)
  let create (oldBlock:TypedSSA.block) = {
    stmts = ref [];
    old_block = oldBlock;
    changes = 0
  }

  let finalize (blockState:t) =
    let changed = blockState.changes > 0 in
    let block =
      if changed then
        Block.of_list (List.rev !(blockState.stmts))
      else blockState.old_block
    in
    block, changed

  (* add statement to block unless it's a no-op *)
  let add_stmt blockState stmtNode =
    if not (TypedSSA.is_empty_stmt stmtNode) then
      blockState.stmts := stmtNode :: !(blockState.stmts)

  let add_stmt_list blockState stmts =
    List.iter (add_stmt blockState) stmts

  let add_block blockState block =
    Block.iter_forward (add_stmt blockState) block

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
    add_stmt blockState (process_update blockState stmtNode update)
end
open BlockState

let exp_update_to_str = function
  | NoChange -> "NoChange"
  | Update e -> "Update: " ^ (TypedSSA.exp_to_str e)
  | UpdateWithStmts (e, _)-> "UpdateStmts: " ^ (TypedSSA.exp_to_str e)
  | UpdateWithBlock (e, _) -> "UpdateBlock: " ^ (TypedSSA.exp_to_str e)

let stmt_update_to_str = function
  | NoChange -> "NoChange"
  | Update e -> "Update: " ^ (TypedSSA.stmt_node_to_str e)
  | UpdateWithStmts (e, _)-> "UpdateStmts: " ^ (TypedSSA.stmt_node_to_str e)
  | UpdateWithBlock (e, _) -> "UpdateBlock: " ^ (TypedSSA.stmt_node_to_str e)



module Mk(R: SIMPLE_TRANSFORM_RULES) = struct
  let dir = R.dir

  type context = R.context
  let init = R.init
  let finalize = R.finalize

  let transform_value blockState cxt vNode =
    process_update blockState vNode (R.value cxt vNode)

  let rec transform_values blockState cxt vNodes = match vNodes with
    | [] -> []
    | v::vs ->
      let v' = transform_value blockState cxt v in
      let vs' = transform_values blockState cxt vs in
      (* check for memory equality of returned results to avoid
         creating redundant cons cells
      *)
      if (v == v') && (vs == vs') then vNodes
      else v'::vs'

  let transform_optional_values blockState cxt = function
    | None -> None
    | Some vNodes -> Some (transform_values blockState cxt vNodes)

  let transform_phi blockState cxt phiNode =
    let version = blockState.changes in
    let left' = transform_value blockState cxt phiNode.phi_left in
    let right' = transform_value blockState cxt phiNode.phi_right in
    let phiNode' =
      if blockState.changes <> version then
        {phiNode with phi_left = left'; phi_right = right' }
      else phiNode
    in
    process_update blockState phiNode' (R.phi cxt phiNode')

  let transform_phi_list blockState cxt phiNodes =
    List.map (transform_phi blockState cxt) phiNodes

  let transform_exp blockState cxt expNode =
    let version = blockState.changes in
    let changed () = blockState.changes <> version in
    let expNode' = match expNode.exp with
    | Values vs ->
      let vs' = transform_values blockState cxt vs in
      if changed() then {expNode with exp = Values vs'} else expNode

    | Arr elts ->
      let elts' = transform_values blockState cxt elts in
      if changed() then {expNode with exp=Arr elts'}
      else expNode

    | Cast(t, v) ->
      let v' = transform_value blockState cxt v in
      if changed() then {expNode with exp=Cast(t,v')}
      else expNode

    | Call (typedFn, args) ->
      let args' = transform_values blockState cxt args in
      if changed() then {expNode with exp=Call(typedFn, args')}
      else expNode

    | PrimApp (prim, args) ->
      let args' = transform_values blockState cxt args in
      if changed() then {expNode with exp=PrimApp(prim,args')}
      else expNode

    | Adverb({Adverb.fixed_args; init; axes} as adverbInfo, args) ->
      let fixedArgs' = transform_values blockState cxt fixed_args in
      let axes' = transform_values blockState cxt axes in
      let init' = transform_optional_values blockState cxt init in
      let args' = transform_values blockState cxt args in
      if changed() then
        let adverbInfo' = { adverbInfo with
          Adverb.fixed_args = fixedArgs';
          init = init';
          axes = axes'
        }
        in
        { expNode with exp = Adverb(adverbInfo', args') }
      else expNode
      in
    BlockState.process_update blockState expNode' (R.exp cxt expNode')

  let rec transform_block cxt (block:block) =
    let blockState = BlockState.create block in
    let folder stmtNode : unit =
      let stmtUpdate = stmt cxt blockState stmtNode in
      BlockState.process_stmt_update blockState stmtNode stmtUpdate
    in
    (match R.dir with
      | Forward -> Block.iter_forward folder block
      | Backward -> Block.iter_backward folder block
    );
    BlockState.finalize blockState


  and stmt cxt blockState stmtNode =
    let oldV = blockState.changes in
    let sub_block_changed () =
      blockState.changes <- blockState.changes + 1
    in
    let changed () =
      blockState.changes <> oldV
    in
    let stmtNode' = match stmtNode.stmt with
    | Set (ids, rhsExpNode) ->
      (*let lhs = transform_value blockState cxt lhs in*)
      let rhsExpNode = transform_exp blockState cxt rhsExpNode in
      if changed() then {stmtNode with stmt=Set(ids, rhsExpNode) }
      else stmtNode

    | SetIdx (lhs, indices, rhs) ->
      let lhs = transform_value blockState cxt lhs in
      let indices = transform_values blockState cxt indices in
      let rhs =  transform_exp blockState cxt rhs in
      if changed() then
        {stmtNode with stmt=SetIdx(lhs, indices, rhs)}
      else stmtNode
    | If (cond, tBlock, fBlock, merge) ->
      let cond' = transform_value blockState cxt cond  in
      let tBlock', tChanged = transform_block cxt tBlock in
      if tChanged then sub_block_changed();
      let fBlock', fChanged = transform_block cxt fBlock in
      if fChanged then sub_block_changed();
      let merge' : TypedSSA.phi_nodes =
        transform_phi_list blockState cxt merge
      in
      if changed() then
        { stmtNode with stmt = If(cond', tBlock', fBlock', merge')  }
      else stmtNode
    | WhileLoop (testBlock, testVal, body, header) ->
      let testBlock', testChanged = transform_block cxt testBlock in
      if testChanged then sub_block_changed ();
      let testVal' = transform_value blockState cxt testVal in
      let bodyBlock', bodyChanged = transform_block cxt body in
      if bodyChanged then sub_block_changed ();
      let header' = transform_phi_list blockState cxt header in
      if changed() then
        { stmtNode with
            stmt= WhileLoop(testBlock', testVal', bodyBlock', header')
        }
      else stmtNode
    in
    match R.stmt cxt stmtNode' with
      | NoChange when changed() -> Update stmtNode'
      | update -> update

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
  let transform_fn fn =
    let cxt = R.init fn in
    set_context cxt;
    let body', changed = transform_block cxt fn.body in
    let fn' = { fn with body = body'} in
    match R.finalize cxt fn' with
      | NoChange -> fn', changed
      | Update fn'' -> fn'', true
      | UpdateWithStmts (fn'', stmts) ->
        { fn'' with body = Block.append (Block.of_list stmts) body'}, true
      | UpdateWithBlock (fn'', block) ->
        { fn'' with body = Block.append block body' }, true
end

