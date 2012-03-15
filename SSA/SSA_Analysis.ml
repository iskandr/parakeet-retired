(* pp: -parser o pa_macro.cmo *)

open Base
open PhiNode
open TypedSSA

type direction = Forward | Backward

(* 'a = environment, *)
(* 'b = information about value nodes *)
(* 'c = information about exp nodes *)
type ('a, 'b) helpers = {
  eval_block : 'a -> block -> 'a * bool;
  eval_stmt : 'a -> stmt_node -> 'a option;
  eval_values : 'a -> value_node list -> 'b list;

  iter_exp_children : 'a -> exp_node -> unit;
  iter_values : 'a -> value_node list -> unit;
}

module type ANALYSIS = sig
  type env
  type exp_info
  type value_info

  val dir : direction

  (* should analysis be repeated until environment stops changing? *)
  val iterative : bool

  val init : fn -> env
  val value : env -> value_node -> value_info

  val exp : env -> exp_node -> (env, value_info) helpers -> exp_info
  val stmt : env -> stmt_node -> (env, value_info) helpers -> env option

  val phi_set : env -> ID.t -> value_info -> env option
  val phi_merge : env -> ID.t -> value_info -> value_info -> env option
end

module MkEvaluator(A : ANALYSIS) = struct
  let rec eval_block initEnv block =
    let changed = ref false in
    let fold_stmts env stmtNode =
      match A.stmt env stmtNode helpers with
      | Some env' -> changed := true; env'
      | None -> env
    in
    let env' =
      match A.dir with
      | Forward -> Block.fold_forward fold_stmts initEnv block
      | Backward -> Block.fold_backward fold_stmts initEnv block
    in
    env', !changed

  and eval_loop_header ?(changed=false) envOut envIn = function
    | [] -> envOut, changed
    | phiNode::rest ->
      let valInfo = A.value envIn phiNode.phi_left in
      let currEnvOut = A.phi_set envOut phiNode.phi_id valInfo in
      let envOut' = Option.default envOut currEnvOut in
      let currChanged = changed || Option.is_some currEnvOut in
      eval_loop_header ~changed:currChanged envOut' envIn rest

  and eval_phi_nodes ?(changed=false) envOut envLeft envRight = function
    | [] -> if changed then Some envOut else None
    | phiNode::rest ->
      let leftInfo = A.value envLeft phiNode.phi_left in
      let rightInfo = A.value envRight phiNode.phi_right in
      let currEnvOut =
        A.phi_merge envOut phiNode.phi_id leftInfo rightInfo
      in
      let envOut' = Option.default envOut currEnvOut in
      let currChanged = changed || Option.is_some currEnvOut in
      eval_phi_nodes ~changed:currChanged envOut' envLeft envRight rest

  and default_stmt env stmtNode = match stmtNode.stmt with
    (* by default don't do anything to the env *)
    | Set (ids, rhs) ->
      (* evaluate rhs for possible side effects *)
      let _  = A.exp env rhs helpers in
      None
    | If(cond, tBlock, fBlock, merge) ->
      ignore (A.value env cond);
      let tEnv, tChanged = eval_block env tBlock in
      let fEnv, fChanged = eval_block tEnv fBlock in
      eval_phi_nodes ~changed:(tChanged || fChanged) fEnv tEnv fEnv merge
    | WhileLoop(condBlock, condVal, body, header) ->
      if A.iterative then (
        let maxIters = 100 in
        let iter = ref 0 in
        let headerEnv, headerChanged = eval_loop_header env env header in
        let condEnv, condChanged = eval_block headerEnv condBlock in
        (* evaluate for side effects *)
        ignore (A.value condEnv condVal);
        let loopEnv = ref condEnv in
        let changed = ref true in
        while !changed do
          iter := !iter + 1;
          if !iter > maxIters then
            failwith $ "loop analysis failed to terminate"
          ;
          let bodyEnv, bodyChanged = eval_block !loopEnv body in
          let phiEnv, phiChanged =
            match eval_phi_nodes bodyEnv headerEnv bodyEnv header with
            | Some env -> env, true
            | None -> bodyEnv, false
          in
          let condEnv, condChanged = eval_block phiEnv condBlock in
          loopEnv := condEnv;
          changed := bodyChanged || phiChanged || condChanged
        done;
        if !iter > 1 then Some !loopEnv else None
      )
      else (
        let headerEnv, headerChanged =
          match eval_phi_nodes env env env header with
          | None -> env, false
          | Some env' -> env', true
        in
        let condEnv, condChanged = eval_block headerEnv condBlock in
        ignore (A.value condEnv condVal);
        let bodyEnv, bodyChanged = eval_block condEnv body in
        let changed = headerChanged || condChanged || bodyChanged in
        if changed then Some bodyEnv else None
      )
    | SetIdx (lhs, indices, rhs) ->
      ignore (A.value env lhs);
      iter_values env indices;
      ignore (A.exp env rhs);
      None

  and iter_exp_children env expNode =
    match expNode.exp with
    | Cast(_, v) -> ignore $ A.value env v
    | Call(_, xs)
    | PrimApp(_,xs)
    | Values xs
    | Arr xs -> iter_values env xs
    | Adverb ({Adverb.fixed_args; init; axes; array_args}) ->
      iter_values env fixed_args;
      iter_values env axes;
      match init with Some inits -> iter_values env inits | None -> ();
      iter_values env array_args
  and iter_values env = function
    | [] -> () | v::vs -> let _ = A.value env v in iter_values env vs
  and eval_values env = function
    | [] -> [] | v::vs -> (A.value env v) :: (eval_values env vs)
  and helpers = {
    eval_block = eval_block;
    eval_stmt = default_stmt;
    eval_values = eval_values;
    iter_values = iter_values;
    iter_exp_children = iter_exp_children;
  }

  let eval_fn fn =
    let env = A.init fn in
    let env', _ = eval_block env fn.body in
    env'
end
