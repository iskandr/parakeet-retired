open Base 
open SSA 

type direction = Forward | Backward

type 'a update =
  | NoChange 
  | Update of 'a 
  | UpdateWithStmts of 'a * (stmt_node list)
  | UpdateWithBlock of 'a * block

module type TRANSFORM_RULES = sig
  type env 
  val init : fundef -> env 
  val dir : direction 
  val stmt : env -> stmt_node -> (stmt_node list) option  
  val exp : env -> exp_node -> exp_node update 
  val value : env -> value_node -> value_node update 
end

module DefaultRules (E : SSA_Analysis.ENV) = struct
  type env = E.t
  let init = E.init 
  let dir = Forward
  let stmt _ _ = None 
  let exp _ _ = NoChange
  let value _ _ = NoChange 
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
  type env = R.env 
  let rec transform_block env block = 
    let blockState = BlockState.create() in  
    let n = SSA.block_length block in
    (match R.dir with 
    | Forward -> 
        for i = 0 to n - 1 do 
          transform_stmt blockState env (block_idx block i)
        done 
    | Backward ->  
        for i = n-1 downto 0 do 
          transform_stmt blockState env (block_idx block i)
        done  
    );
    BlockState.finalize blockState 
     
  and transform_stmt blockState env stmtNode = 
    let oldNumChanges = blockState.changes in 
    let stmtNode' = match stmtNode.stmt with 
    | Set (ids, rhsExpNode) ->
      let rhsExpNode' = transform_exp blockState env rhsExpNode in  
      if oldNumChanges <> blockState.changes then 
        {stmtNode with stmt=Set(ids, rhsExpNode') }
      else stmtNode 
     
    | SetIdx (id, indices, rhsVal) ->
        let indices' = transform_values blockState env indices in
        let rhsVal' =  transform_value blockState env rhsVal in
        if oldNumChanges <> blockState.changes  then 
          {stmtNode with stmt=SetIdx(id, indices', rhsVal')} 
        else stmtNode 
    | If (v, tBlock, fBlock, ifGate) -> 
        failwith "if not implemented" 
    | WhileLoop (condBlock, condId, bodyBlock, loopGate) -> 
        failwith "loop not implemented" 
  in
  BlockState.process_stmt_update blockState stmtNode' (R.stmt env stmtNode')
     
  and transform_exp blockState env expNode = 
    let oldNumChanges = blockState.changes in 
    let expNode' = match expNode.exp with 
    | Values vs ->
      let vs' = transform_values blockState env vs in
      if blockState.changes <> oldNumChanges then 
        {expNode with exp = Values vs'}
      else expNode 
    | App(fn,args) ->
      let fn'= transform_value blockState env fn in 
      let args' = transform_values blockState env args in
      if blockState.changes <> oldNumChanges then 
        {expNode with exp = App(fn', args') } 
      else expNode
  in 
  BlockState.process_update blockState expNode' (R.exp env expNode')    

  and transform_values blockState env ?(revAcc=[])  = function  
  | [] -> List.rev revAcc 
  | v::vs ->
      let oldNumChanges = blockState.changes in 
      let v' = transform_value blockState env v in 
      transform_values
        blockState  
        env 
        ~revAcc:(v' :: revAcc) 
        vs
          
  and transform_value blockState env vNode = 
    BlockState.process_update blockState vNode (R.value env vNode)

  (* these exist so we can access the transform env from outside functions, *)
  (* in case it contains useful information *) 
  let globalEnv = ref None 
  let set_env e = globalEnv := Some e
  let get_env () = match !globalEnv with 
    | None -> assert false 
    | Some e -> e 
   
  let transform_fundef fundef =
    let env = R.init fundef in
    set_env env;     
    let body', changed = transform_block env fundef.body in
    {fundef with body = body'}, changed 
end 

module DefaultTransformation (E: SSA_Analysis.ENV) = 
  MkTransformation(DefaultRules(E))
