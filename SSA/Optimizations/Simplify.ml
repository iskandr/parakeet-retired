(* pp: -parser o pa_macro.cmo *)

open Base
open Type
open TypedSSA
open SSA_Transform

open FindUseCounts

module SimplifyRules = struct
  let dir = Forward

  type context = {
    constants: TypedSSA.value ConstantLattice.t ID.Map.t;
    copies : FindCopies.CopyLattice.t ID.Map.t;
    use_counts : (ID.t, int) Hashtbl.t;
    types : Type.t ID.Map.t;
  }

  let init fn = {
      constants = FindConstants.find_constants fn;
      copies = FindCopies.find_copies fn;
      use_counts = FindUseCounts.find_fundef_use_counts fn;
      types = fn.tenv;
    }

  (* since all outputs are considered used, dummy assignments leading to an *)
  (* output don't get cleaned up. This final step gets rid of stray assignments*)
  (* at the end of a function *)
  let finalize cxt fundef =
    let inputIdSet = ID.Set.of_list fundef.input_ids in
    let aux id =
      match ID.Map.find_option id cxt.copies with
        | None -> failwith $ "[Simplify] Couldn't find " ^ (ID.to_str id)
        | Some (FindCopies.CopyLattice.Copy prevId) ->
          (* an ID can't be both input and output *)
           if ID.Set.mem prevId inputIdSet then id else prevId
        | Some _ -> id
    in
    let outputIds = List.map aux fundef.output_ids in
    if List.eq_elts outputIds fundef.output_ids then NoChange
    else Update {fundef with output_ids = outputIds }

  let is_live cxt id =
    Hashtbl.mem cxt.use_counts id && Hashtbl.find cxt.use_counts id > 0


  let stmt cxt stmtNode = match stmtNode.stmt with
    | Set (ids, ({exp=Values vs} as expNode)) ->
      let pairs = List.combine ids vs in
      let  livePairs, deadPairs =
        List.partition (fun (id,_) -> is_live cxt id) pairs
      in
      if deadPairs = [] then NoChange
      else if livePairs = [] then Update TypedSSA.empty_stmt
      else
        let liveIds, liveValues = List.split livePairs in
        let rhs = {
          expNode with exp=Values liveValues; 
          exp_types = List.map (fun vNode -> vNode.value_type) liveValues
        } 
        in
        Update (TypedSSA.set ?src:stmtNode.stmt_src liveIds rhs)
    | Set (ids, exp) ->
        let rec any_live = function
          | [] -> false
          | id::rest -> (is_live cxt id) || any_live rest
        in
        if any_live ids then NoChange
        else Update TypedSSA.empty_stmt

    | If (condVal, tBlock, fBlock, merge) ->
      let get_type id = ID.Map.find id cxt.types in
      begin match condVal.value with
        | Num (ParNum.Bool b) ->
          let branch = if b then tBlock else fBlock in
          let ids, valNodes = PhiNode.collect_phi_values b merge in
          let types = List.map get_type ids in
          let src = stmtNode.stmt_src in
          let expNode =
            TypedSSA.exp ?src types (TypedSSA.Values valNodes)
          in
          let setNode = TypedSSA.set ?src:stmtNode.stmt_src ids expNode in
          UpdateWithBlock(setNode, branch)
        | _ -> NoChange
      end
    | WhileLoop (testBlock, testVal, body, header) -> NoChange
    | _ -> NoChange

  let exp cxt expNode = match expNode.exp with
    | Cast(t, {value=Num n}) ->
      let n' = ParNum.coerce n (Type.elt_type t) in
      Update (TypedSSA.ExpHelpers.val_exp ?src:expNode.exp_src t (Num n'))
    | _ -> NoChange

  let phi cxt phiNode = NoChange

  let value cxt valNode = match valNode.value with
    | Var id ->
      (* there's only one value of type NoneT, so use it directly *) 
      if valNode.value_type = Type.NoneT then Update {valNode with value = NoneVal } 
      else 
      begin match ID.Map.find_option id cxt.constants with
        | Some ConstantLattice.Const v ->
          Update {valNode with value = v }
        | Some _
        | None ->
          (match ID.Map.find id cxt.copies with
            | FindCopies.CopyLattice.Copy prevId ->
              let valNode' = {valNode with value = Var prevId} in
              Update valNode'
            | _ -> NoChange
          )
      end
    | _ -> 
      NoChange
end

module Simplifer = SSA_Transform.Mk(SimplifyRules)

let simplify_fn (_ : FnTable.t) fn = Simplifer.transform_fn fn

