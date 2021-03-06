(* pp: -parser o pa_macro.cmo *)

open Base
open Type
open TypedSSA
open SSA_Transform

let do_inline fn argVals =
  let idSet : ID.t MutableSet.t = FindBindingSet.fn_bindings fn in
  let helper id accMap =
    let prefix : string = ID.get_original_prefix id in
    ID.Map.add id (ID.gen_named prefix) accMap
  in
  let replaceMap = MutableSet.fold helper idSet ID.Map.empty in
  let freshFn, _ = Replace.replace_fn replaceMap fn in
  let rhs = TypedSSA.exp freshFn.fn_input_types (Values argVals) in
  let argAssignments = TypedSSA.set freshFn.input_ids rhs  in
  let outputIds = freshFn.output_ids in
  let outputTypes = freshFn.fn_output_types in
  let outputValNodes =
    List.map2 (fun id t -> TypedSSA.var t id) outputIds outputTypes
  in
  let outputExp = TypedSSA.exp outputTypes (Values outputValNodes) in
  (* list of new ids and their types-- ignore types missing from tenv *)
  let typesList : (ID.t * Type.t) list =
    MutableSet.fold
      (fun oldId accList ->
          if ID.Map.mem oldId fn.tenv then
            let newId = ID.Map.find oldId replaceMap in
            (newId, ID.Map.find oldId fn.tenv)::accList
          else accList
      )
      idSet
      []
  in
  let body' = Block.append (Block.singleton argAssignments) freshFn.body in
  body', outputExp, typesList

let lookup (id:FnId.t)  : TypedSSA.fn option = 
  FnTable.find_option id (FnManager.get_typed_function_table())

module Inline_Rules = struct
  type context = (Type.t ID.Map.t) ref
  let init fn = ref fn.tenv
  let finalize _ _ = NoChange
  let dir = Forward

  let rec add_types_list envRef = function
    | [] -> envRef
    | (id,t)::rest ->
        envRef := ID.Map.add id t !envRef;
        add_types_list envRef rest

  let stmt envRef stmtNode = NoChange

  let exp envRef expNode = match expNode.exp with
    | Call (fnId, args) ->
      begin match lookup fnId with
        | None -> NoChange
        | Some fn ->
          (* make sure arity lines up *)
          if List.length fn.input_ids <> List.length args then NoChange
          else
          let inlineBlock, outputExp, typesList = do_inline fn args in
          let _ = add_types_list envRef typesList in
          IFDEF DEBUG THEN
            assert (outputExp.exp_types = expNode.exp_types);
          ENDIF;
          let expNode' = {outputExp with exp_src=expNode.exp_src } in
          IFDEF DEBUG THEN
            Printf.printf "Inline updated exp: %s => %s \n"
              (TypedSSA.exp_node_to_str expNode)
              (TypedSSA.exp_node_to_str expNode');
          ENDIF;
          UpdateWithBlock(expNode', inlineBlock)
       end
    | _ -> NoChange

  let phi env phiNode = NoChange
  let value env valNode = NoChange
end

module Inliner = SSA_Transform.Mk(Inline_Rules) 

let run_fn_inliner fn =
  let fn', changed = Inliner.transform_fn fn in
  let tenv' = !(Inliner.get_context ()) in
  {fn' with tenv = tenv' }, changed