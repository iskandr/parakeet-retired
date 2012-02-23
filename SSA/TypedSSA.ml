(* pp: -parser o pa_macro.cmo *)

open Base
open Printf

(* bottle these up in a module so we can parameterize SSA.Make *)
module CoreLanguage = struct
  type value = Var of ID.t | Num of ParNum.t

  type value_node = {
    value : value;
    value_type : Type.t;
    value_src : SrcInfo.t option;
  }

	type value_nodes = value_node list
	type adverb_info = (FnId.t, value_nodes, value_nodes) Adverb.info

	type exp =
	  | Values of value_nodes
	  | Arr of value_nodes
	  | Call of FnId.t * value_nodes
	  | PrimApp of Prim.t * value_nodes
	  | Adverb of adverb_info * value_nodes
    | Cast of Type.t * value_node

	type exp_node = {
	  exp: exp;
	  exp_src : SrcInfo.t option;
	  exp_types : Type.t list
	}

  type phi_node = value_node PhiNode.t
  type phi_nodes = phi_node list

  type stmt =
    | Set of ID.t list * exp_node
    | SetIdx of value_node * value_node list * exp_node
    | If of value_node * block * block * phi_nodes
    (* testBlock, testVal, body, loop header, loop exit *)
    | WhileLoop of block * value_node * block *  phi_nodes

  and stmt_node = {
    stmt: stmt;
    stmt_src: SrcInfo.t option;
    stmt_id : StmtId.t;
  }
  and block = stmt_node Block.t


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

  type tenv = Type.t ID.Map.t
  type fn = {
    body: block;
    tenv : tenv;
    input_ids: ID.t list;
    output_ids: ID.t list;
    fn_input_types : Type.t list;
    fn_output_types : Type.t list;
    fn_id : FnId.t;


  }

end
include CoreLanguage

module PrettyPrinters = struct
  let value_to_str = function
    | Var id -> ID.to_str id
    | Num n -> ParNum.to_str n

  let value_node_to_str valNode =
    sprintf "%s : %s"
      (value_to_str valNode.value)
      (Type.to_str valNode.value_type)

  let value_nodes_to_str valNodes =
    String.concat ", " (List.map value_node_to_str valNodes)

  let adverb_info_to_str info =
    Adverb.info_to_str FnId.to_str value_nodes_to_str value_nodes_to_str info

  let exp_to_str = function
    | Values vs -> sprintf "values(%s)" (value_nodes_to_str vs)
    | Arr elts -> sprintf "array(%s)" (value_nodes_to_str elts)
    | Call (fnId, args) ->
      sprintf "%s(%s)" (FnId.to_str fnId) (value_nodes_to_str args)
    | PrimApp (p, args) ->
      sprintf "prim[%s](%s)" (Prim.to_str p) (value_nodes_to_str args)
    | Adverb (info, args) ->
      Printf.sprintf "%s(%s)"
        (adverb_info_to_str info)
        (value_nodes_to_str args)
    | Cast (t, x) ->
      sprintf "cast[%s](%s)" (Type.to_str t) (value_node_to_str x)
  let exp_node_to_str { exp } = exp_to_str exp

  let phi_node_to_str = PhiNode.to_str value_node_to_str
  let phi_nodes_to_str phiNodes =
    let lines = List.map phi_node_to_str phiNodes in
    String.concat "\n" lines

  let rec stmt_to_str stmt =
    Base.wrap_str $ match stmt with
    | Set (ids, rhs) ->
      sprintf "%s = %s" (ID.list_to_str ids) (exp_node_to_str rhs)
    | SetIdx (arr, indices, rhs) ->
      sprintf "%s[%s] = %s"
      (value_node_to_str arr)
      (String.concat ", " (List.map value_node_to_str indices))
      (exp_node_to_str rhs)
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
  and stmt_node_to_str {stmt} = stmt_to_str stmt
  and block_to_str block =
    Block.to_str stmt_node_to_str block

  let typed_id_to_str tenv id =
    (ID.to_str id) ^ " : " ^ (Type.to_str (ID.Map.find id tenv))

  let typed_ids_to_str tenv ids =
    String.concat ", " (List.map (typed_id_to_str tenv) ids)

  let fn_to_str (fundef: fn) =
    let name = FnId.to_str fundef.fn_id in
    let inputs = typed_ids_to_str fundef.tenv fundef.input_ids in
    let outputs = typed_ids_to_str fundef.tenv fundef.output_ids in
    let body = block_to_str fundef.body in
    Base.wrap_str (sprintf "def %s(%s)=>(%s):\n%s" name inputs outputs body)
end
include PrettyPrinters


let wrap_value ?src value ty =
      { value = value; value_src = src; value_type = ty }
let wrap_exp valNode =
    { exp = Values [valNode]; exp_src = None; exp_types = [valNode.value_type]}
let wrap_stmt ?src stmt =
    { stmt = stmt; stmt_src = src; stmt_id = StmtId.gen() }

let is_empty_exp = function Values [] -> true | _ -> false
let is_empty_exp_node {exp} = is_empty_exp exp
let is_empty_stmt {stmt} = match stmt with
  | Set([], expNode)-> is_empty_exp_node expNode
  | _ -> false

module ValueHelpers = struct
    (* get the id of a variable value node *)
  let get_id valNode = match valNode.value with
    | Var id -> id
    | other -> failwith $ Printf.sprintf
       "[SSA->get_id] expected variable, received %s"
       (value_to_str other)

  let var ?src ty (id:ID.t) : value_node =
    { value = Var id; value_src = src; value_type = ty }

  let num ?src n =
    let ty = Type.ScalarT (ParNum.type_of n) in
    { value = Num n; value_type = ty; value_src = src }

  let bool ?src b = num ?src (ParNum.Bool b)
  let int32 ?src i = num ?src (ParNum.coerce_int i Type.Int32T)
  let float32 ?src f = num ?src (ParNum.Float32 f)
  let float64 ?src f = num ?src (ParNum.Float64 f)
  let is_const {value} = match value with | Num _ -> true | _ -> false

  let is_const_int {value} =
    match value with | Num n -> ParNum.is_int n | _ -> false

  let get_const {value} = match value with
    | Num n -> n
    | other ->
      failwith $ Printf.sprintf "Expected constant, got %s" (value_to_str other)
  let get_const_int valNode = ParNum.to_int (get_const valNode)
end
include ValueHelpers

module FnHelpers = struct
	let mk_fn ?name ~tenv ~input_ids ~output_ids ~body : fn =
	  let inTypes = List.map (fun id -> ID.Map.find id tenv) input_ids in
	  let outTypes = List.map (fun id -> ID.Map.find id tenv) output_ids in
	  let fnId = match name with
	    | Some name -> FnId.gen_named name
	    | None -> FnId.gen()
	  in
	  {
	    body = body;
	    tenv = tenv;
	    input_ids = input_ids;
	    output_ids = output_ids;
	    fn_input_types = inTypes;
	    fn_output_types = outTypes;
	    fn_id = fnId
	  }
	let fn_builder
	      ?name
	      ~(input_types : Type.t list)
	      ~(output_types : Type.t list)
	      ?(local_types = [])
	      (construct : value_nodes * value_nodes * value_nodes -> stmt_node list) =
	  IFDEF DEBUG THEN
	    Printf.printf
	      "[SSA_Helpers.mk_fn] name: %s, input_types = %s, output_types = %s\n%!"
	      (match name with None -> "<none>" | Some name -> name)
	      (Type.type_list_to_str input_types)
	      (Type.type_list_to_str output_types)
	    ;
	  ENDIF;
	  (* inputs *)
	  let nInputs = List.length input_types in
	  let inputIds = ID.gen_named_list "input" nInputs in
	  let inputs = List.map2 (fun t id -> var t id) input_types inputIds in
	  (* outputs *)
	  let nOutputs = List.length output_types in
	  let outputIds = ID.gen_named_list "output" nOutputs in
	  let outputs = List.map2 (fun t id -> var  t id) output_types outputIds in
	  (* locals *)
	  let nLocals = List.length local_types in
	  let localIds = ID.gen_named_list "temp" nLocals in
	  let locals = List.map2 (fun t id -> var t id) local_types localIds in
	  let body = Block.of_list (construct (inputs, outputs, locals)) in
	  let tenv =
	    ID.Map.of_lists
	      (inputIds @ outputIds @ localIds)
	      (input_types @ output_types @ local_types)
	  in
	  mk_fn ?name ~tenv ~input_ids: inputIds ~output_ids: outputIds ~body

	  let find_fn_src_info { body } = get_block_src_info body

	  let input_arity { input_ids } = List.length input_ids
	  let output_arity { output_ids } = List.length output_ids
	  let input_types { fn_input_types } = fn_input_types
	  let output_types { fn_output_types } = fn_output_types
	  let fn_id { fn_id } = fn_id
end
include FnHelpers


