(* pp: -parser o pa_macro.cmo *)

open Base
open Printf
open PhiNode
open TypedSSA

type errors = (SrcInfo.t option * string) Queue.t
type tenv = Type.t ID.Map.t

let check_value (errorLog:errors)(tenv:tenv) (defined : ID.Set.t) vNode : unit =
 let err msg = Queue.add (vNode.value_src, msg) errorLog in
  match vNode.value with
  | Var id ->
    if not $ ID.Set.mem id defined then
      err $ sprintf "attempting to use undefined variable %s" (ID.to_str id)
    else if not $ ID.Map.mem id tenv then
      err $ sprintf "type not found for %s" (ID.to_str id)
    else
      let t = ID.Map.find id tenv in (
      if t <> vNode.value_type then
        err $ sprintf
          "type annotation %s for variable %s does not match its type %s"
          (Type.to_str vNode.value_type)
          (ID.to_str id)
          (Type.to_str t)
      )
  | Num _ ->
    if not $ Type.is_number vNode.value_type then
      err "number annotated with non-numeric type"

let check_value_list errorLog tenv defined values : unit =
  List.iter (check_value errorLog tenv defined) values

let rec check_stmt
  (errorLog : errors)
  (tenv : tenv)
  (defined : ID.Set.t)
  (stmtNode:stmt_node) : ID.Set.t =
  let err msg = Queue.add (stmtNode.stmt_src, msg) errorLog in
  match stmtNode.stmt with
  | Set (ids, rhs) ->
      check_exp errorLog tenv defined rhs;
      let type_exists (id :ID.t) : bool =
        let found = ID.Map.mem id tenv in
        if not found then
          err $ Printf.sprintf
            "no type found for variable %s on lhs of assignment"
            (ID.to_str id)
        ;
        found
      in
      if List.for_all type_exists ids then (
        let lhsTypes = List.map (fun id -> ID.Map.find id tenv) ids in
        let rhsTypes = rhs.exp_types in
        if lhsTypes <> rhsTypes then (
          let lhsCount = List.length ids in
          let rhsCount = List.length rhs.exp_types in
          err $ sprintf
            "type error in assignment: %s %s type %s, but rhs %s %s type %s %s"
            (ID.list_to_str ids)
            (if lhsCount > 1 then "have" else "has")
            (Type.type_list_to_str lhsTypes)
            (TypedSSA.PrettyPrinters.exp_to_str rhs.exp)
            (if rhsCount > 1 then  "have" else "has")
            (Type.type_list_to_str rhsTypes)
            (if lhsCount <> rhsCount then "(arity mismatch)" else "")
         )
      );
      ID.Set.add_list ids defined
  | SetIdx ({value=Var arrId}, indices, rhs) ->
      if not $ ID.Set.mem arrId defined then
        err $ sprintf
          "attempting to set array index on undefined variable %s"
          (ID.to_str arrId)
      else (
        let lhsType = ID.Map.find arrId tenv in
        if Type.is_scalar lhsType then
          err $ Printf.sprintf "cannot index into scalar %s" (ID.to_str arrId)
      );
      check_value_list errorLog tenv defined indices;
      check_exp errorLog tenv defined rhs;
      defined
  | SetIdx _ ->
      err $ "Expected lhs of setidx to be a variable";
      defined
  | If (test, tBlock, fBlock, phiNodes) ->
      check_value errorLog tenv defined test;
      if test.value_type <> Type.bool then
        err $ "If statement requires boolean condition"
      ;
      let defined = check_block errorLog tenv defined tBlock in
      let defined = check_block errorLog tenv defined fBlock in
      let phiIds = PhiNode.collect_phi_ids phiNodes in
      ID.Set.add_list phiIds defined

  | WhileLoop (testBlock, testVal, body, phiNodes) ->
      let phiIds = List.map (fun phiNode -> phiNode.phi_id) phiNodes in
      let defined = ID.Set.add_list phiIds defined in
      let defined = check_block errorLog tenv defined testBlock in
      check_value errorLog tenv defined testVal;
      check_block errorLog tenv defined body

and check_exp errorLog tenv (defined : ID.Set.t) (expNode : exp_node) : unit =
  let err msg = Queue.add (expNode.exp_src, msg) errorLog in
  match expNode.exp with
  | Values vs
  | Arr vs -> check_value_list errorLog tenv defined vs
  | Cast (t, v) ->
      check_value errorLog tenv defined v;
      if expNode.exp_types <> [t] then
        err $ sprintf "context expects type %s but cast is to type %s"
          (Type.type_list_to_str expNode.exp_types)
          (Type.to_str t)
  | Call (typedFn, args) -> ()
  | PrimApp (typedPrim, args) -> ()
  | Adverb adverbInfo  -> ()


and check_block
      (errorLog : errors)
      (tenv : Type.t ID.Map.t)
      (defined : ID.Set.t)
      (block : TypedSSA.block) : ID.Set.t =
  Block.fold_forward
    (fun accDefined stmtNode -> check_stmt errorLog tenv accDefined stmtNode)
    defined
    block
and check_fn ?(errorLog=Queue.create()) fundef =
  let defined = ID.Set.of_list fundef.input_ids in
  let _ = check_block errorLog fundef.tenv defined fundef.body in
  errorLog

let err_printer = function
  | (None, msg) -> Printf.printf "  * %s\n" msg
  | (Some src, msg) -> Printf.printf "  * %s : %s\n" msg "[source]"

let print_all_errors errorLog =  Queue.iter err_printer errorLog

