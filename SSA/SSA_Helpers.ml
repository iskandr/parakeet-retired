(* pp: -parser o pa_macro.cmo *)

open Base
open SSA

(* if a function contains nothing but a map, extracted the nested
   function being mapped
*)


let mk_fn ?name ?(tenv=ID.Map.empty) ~input_ids ~output_ids ~body : SSA.fn =
  let inTypes =
    List.map (fun id -> ID.Map.find_default id tenv Type.BottomT) input_ids
  in
  let outTypes =
    List.map (fun id -> ID.Map.find_default id tenv Type.BottomT) output_ids
  in
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


(* get the id of a variable value node *)
let get_id valNode = match valNode.value with
  | Var id -> id
  | other -> failwith $ Printf.sprintf
     "[SSA->get_id] expected variable, received %s"
     (value_to_str other)



(***
    helpers for values
 ***)


let mk_val ?src ?(ty=Type.BottomT) (v:value) : value_node =
  { value = v; value_src = src; value_type = ty }

let mk_var ?src ?(ty=Type.BottomT) (id:ID.t) : value_node =
  { value = Var id; value_src = src; value_type = ty }

let mk_op ?src ?ty op = mk_val ?src ?ty (Prim op)

let mk_globalfn ?src ?(ty=Type.BottomT) (id:FnId.t) : value_node=
  { value = GlobalFn id; value_src = src; value_type = ty }

let mk_num ?src ?ty n =
  let ty = match ty with
    | None -> Type.ScalarT (ParNum.type_of n)
    | Some t -> t
  in
  mk_val ?src ~ty (Num n)

let mk_bool ?src b = mk_num ?src ~ty:Type.bool (ParNum.Bool b)

let mk_int32 ?src i =
  mk_num ?src ~ty:Type.int32 (ParNum.coerce_int i Type.Int32T)

let mk_float32 ?src f = mk_num ?src ~ty:Type.float32 (ParNum.Float32 f)

let mk_float64 ?src f = mk_num ?src ~ty:Type.float64 (ParNum.Float64 f)

(***
    helpers for expressions
 ***)

let map_default_types optTypes values =
  match optTypes with
    | None -> List.map (fun vNode -> vNode.value_type) values
    | Some ts -> ts

let mk_app ?src fn args =
  { exp=App(fn,args); exp_src = src; exp_types = [Type.BottomT] }

let mk_primapp ?src prim ~output_types args =
  { exp = PrimApp (prim, args); exp_src = src; exp_types = output_types}

let mk_arr ?src ?types elts =
  let argTypes = map_default_types types elts in
  let resultT = match argTypes with
    | [] -> Type.BottomT
    | Type.BottomT::_ -> Type.BottomT
    | (Type.ScalarT elt_t)::_ -> Type.ArrayT(elt_t, 1)
    | others -> failwith $ Printf.sprintf
        "Invalid array element types: %s"
        (Type.type_list_to_str others)
  in
  { exp=Arr elts; exp_src=src; exp_types = [resultT] }

let mk_val_exp ?src ?ty (v: value) =
  let ty' = match ty with
    | None -> Type.BottomT
    | Some ty -> ty
  in
  { exp=Values [mk_val ?src v]; exp_src=src; exp_types = [ty'] }

let mk_vals_exp ?src ?types ( vs : value list) =
  let valNodes = match types with
    | Some types -> List.map2 (fun v ty -> mk_val ?src ~ty v) vs types
    | None -> List.map (mk_val ?src) vs
  in
  let types' = map_default_types types valNodes in
  { exp = Values valNodes; exp_src = src; exp_types=types' }

let mk_cast ?src t v =
  { exp = Cast(t, v); exp_types = [t]; exp_src = src }

let mk_exp ?src ?types exp =
  (* WARNING--- function calls may need more than 1 return type, in which
     case the default [BottomT] is wrong
  *)
  let types' = match types, exp with
    | Some ts, _ -> ts
    | None, Values vs -> List.fill Type.BottomT vs
    | _ -> [Type.BottomT]
  in
  { exp= exp; exp_types = types'; exp_src = src}

let mk_call ?src fnId outTypes args  =
  { exp = Call(fnId, args); exp_types = outTypes; exp_src=src}




let mk_closure fundef args = {
  closure_fn = fundef.fn_id;
  closure_args = args;
  closure_arg_types = List.map (fun v -> v.value_type) args;
  (*closure_input_types = List.drop (List.length args) fundef.fn_input_types;
  closure_output_types = fundef.fn_output_types;*)
}


(***
     helpers for statements
 ***)

let mk_stmt ?src ?(id=StmtId.gen()) stmt =
  { stmt = stmt; stmt_src = src; stmt_id = id }

let mk_set ?src ids rhs =
  { stmt = Set(ids, rhs);
    stmt_src = src;
    stmt_id = StmtId.gen()
  }

let mk_setidx ?src lhs indices rhs =
  { stmt = SSA.SetIdx(lhs, indices, rhs);
    stmt_src = src;
    stmt_id = StmtId.gen()
  }

(***
   helpers for phi-nodes

 ***)


let mk_phi ?src ?ty id left right =
  {
    phi_id = id;
    phi_left = left;
    phi_right = right;
    phi_type = Option.default Type.BottomT ty;
    phi_src = src;
  }

let empty_phi = mk_phi ID.undefined (mk_var ID.undefined) (mk_var ID.undefined)
let is_empty_phi phiNode = match phiNode.phi_left, phiNode.phi_right with
  | {value=Var idLeft}, {value=Var idRight} ->
      idLeft == ID.undefined && idRight == ID.undefined &&
      phiNode.phi_id == ID.undefined
  | _ -> false


(* make a block of phi nodes merging IDs from the three lists given *)
let rec mk_phi_nodes outIds leftIds rightIds =
  match (outIds, leftIds, rightIds) with
    | [], _, _ | _,[],_ | _,_,[] -> []
    | x::xs, y::ys, z::zs ->
      (mk_phi x (mk_var y) (mk_var z)) :: (mk_phi_nodes xs ys zs)

let rec mk_phi_nodes_from_values outVals leftVals rightVals =
  match (outVals, leftVals, rightVals) with
    | [], _, _ | _, [], _ | _, _, [] -> []
    | x::xs, y::ys, z::zs ->
      (mk_phi (get_id x) y z) :: (mk_phi_nodes_from_values xs ys zs)

(* assume a block contains only phi, collect the IDs and
   either the left or right values
*)
let rec collect_phi_values chooseLeft = function
  | [] -> [], []
  | p::ps ->
    let ids, valNodes = collect_phi_values chooseLeft ps in
    let currVal = if chooseLeft then p.phi_left else p.phi_right in
    p.phi_id :: ids, currVal :: valNodes


(* get the ids from a list of variable value nodes *)
let get_ids vars = List.map get_id vars

let get_fn_id valNode = match valNode.value with
  | GlobalFn fnId -> fnId
  | other -> failwith $
      Printf.sprintf
        "[SSA->get_fn_id] expected global function, received %s"
        (value_to_str other)


let get_fn_ids valNodes = List.map get_fn_id valNodes
let empty_stmt = mk_set [] (mk_vals_exp [])

let is_empty_stmt stmtNode =
  match stmtNode.stmt with
    | Set ([], {exp=Values[]})->true
    | _ -> false

