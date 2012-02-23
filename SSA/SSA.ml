(* pp: -parser o pa_macro.cmo *)

open Base
open Printf


let rec wrap_str s =
  Str.global_substitute (Str.regexp "\n") (fun _ -> "  \n") s

module StmtId = UID.Make(struct let prefix = "stmt" end)

module type CORE_LANGUAGE = sig
  type value
  val value_to_str : value -> string
  type value_node
  val value_node_to_str : value_node -> string
  val value_nodes_to_str : value_node list -> string

  type exp
  val exp_to_str : exp -> string
  type exp_node
  val exp_node_to_str : exp_node -> string
  val exp_nodes_to_str : exp_node list -> string

  val is_empty_exp_node : exp_node -> bool
end

module Mk(L : CORE_LANGUAGE) = struct
  type phi_node = {
    phi_id : ID.t;
    phi_left:  L.value_node;
    phi_right: L.value_node;
    phi_src : SrcInfo.t option;
  }
  type phi_nodes = phi_node list

  let phi_node_to_str phiNode =
      sprintf "%s <- phi(%s, %s)"
        (ID.to_str phiNode.phi_id)
        (value_node_to_str phiNode.phi_left)
        (value_node_to_str phiNode.phi_right)

  let phi_nodes_to_str phiNodes =
    let lines = List.map phi_node_to_str phiNodes in
    String.concat "\n" lines

  let phi ?src id left right =
    { phi_id = id; phi_left = left; phi_right = right; phi_src = src; }

  (* make a block of phi nodes merging the three lists given *)
  let rec phi_nodes ?src outIds leftVals rightVals =
    match outIds, leftVals, rightVals with
    | [], _, _ | _,[],_ | _,_,[] -> []
    | x::xs, y::ys, z::zs -> (phi ?src x y z) :: (phi_nodes ?src xs ys zs)


  (* assume a block contains only phi, collect the IDs and
     either the left or right values
  *)
  let rec collect_phi_values chooseLeft = function
    | [] -> [], []
    | p::ps ->
      let ids, valNodes = collect_phi_values chooseLeft ps in
      let currVal = if chooseLeft then p.phi_left else p.phi_right in
      p.phi_id :: ids, currVal :: valNodes

  type stmt =
    | Set of ID.t list * L.exp_node
    | SetIdx of L.value_node * L.value_node list * L.exp_node
    | If of L.value_node * block * block * phi_nodes
    (* testBlock, testVal, body, loop header, loop exit *)
    | WhileLoop of block * L.value_node * block *  phi_nodes
  and stmt_node = {
    stmt: stmt;
    stmt_src: SrcInfo.t option;
    stmt_id : StmtId.t;
  }
  and block = stmt_node Block.t

  let wrap_stmt ?src stmt =
    { stmt = stmt; stmt_src = src; stmt_id = StmtId.gen() }

  let is_empty_stmt {stmt} = match stmt with
    | Set([], exp_node)-> L.is_empty_exp_node exp_node
    | _ -> false

  let ids_to_str (ids:ID.t list) = String.concat ", " (List.map ID.to_str ids)

  let rec stmt_to_str stmt =
    wrap_str $ match stmt with
    | Set (ids, rhs) -> sprintf "%s = %s" (ids_to_str ids) (exp_node_to_str rhs)
    | SetIdx (arr, indices, rhs) ->
      sprintf "%s[%s] = %s"
      (value_node_to_str arr)
      (String.concat ", " (List.map value_node_to_str indices))
      (exp_to_str rhs)
  | If (cond, tBlock, fBlock, phiNodes) ->
    wrap_str $ sprintf "if %s:\n%s \nelse:\n%s \nmerge:\n%s"
      (value_node_to_str cond)
      (block_to_str tBlock)
      (block_to_str fBlock)
      (phi_nodes_to_str phiNodes)
  | WhileLoop (testBlock, testVal, body, phiNodes) ->
    wrap_str $ sprintf "while %s:\nloop header:\n%s\ndo:\n%s\nmerge:\n%s"
      (block_to_str testBlock)
      (value_node_to_str testVal)
      (block_to_str body)
      (phi_nodes_to_str phiNodes)
  and stmt_node_to_str exp_to_str val_to_str {stmt} = stmt_to_str stmt
  and block_to_str exp_to_str val_to_str block =
    Block.to_str stmt_node_to_str block

  (* search through a block and return the first srcinfo, *)
  (* if one exists. Return None otherwise *)
  let rec get_stmt_src_info {stmt; stmt_src} =
    if stmt_src <> None then stmt_src
    else match stmt with
    | If(_, tBlock, fBlock, _) ->
      let tSrc = get_block_src_info tBlock in
      if tSrc = None then get_block_src_info fBlock
      else tSrc
    | WhileLoop(condBlock, _, body, _) ->
      let condSrc = get_block_src_info condBlock in
      if condSrc = None then get_block_src_info body
      else condSrc
    | _ -> None
  and get_block_src_info block = Block.find_first get_stmt_src_info block
end


