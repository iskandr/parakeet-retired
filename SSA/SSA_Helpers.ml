(* pp: -parser o pa_macro.cmo *)

open Base
open SSA

(* if a function contains nothing but a map, extract the nested
   function being mapped
*)


(* get the id of a variable value node *)
let get_id valNode = match valNode.value with
  | Var id -> id
  | other -> failwith $ Printf.sprintf
     "[SSA->get_id] expected variable, received %s"
     (value_to_str other)


(****************************************************************************)
(*                         Expression Helpers                               *)
(****************************************************************************)

let map_default_types optTypes values =
  match optTypes with
    | None -> List.map (fun vNode -> vNode.value_type) values
    | Some ts -> ts

let app ?src fn args =
  { exp=App(fn,args); exp_src = src; exp_types = [Type.BottomT] }

let primapp ?src prim ~output_types args =
  { exp = PrimApp (prim, args); exp_src = src; exp_types = output_types}

let arr ?src ?types elts =
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

let val_exp ?src ?ty (v: value) =
  let ty' = match ty with
    | None -> Type.BottomT
    | Some ty -> ty
  in
  { exp=Values [wrap_value ?src v]; exp_src=src; exp_types = [ty'] }

let vals_exp ?src ?types ( vs : value list) =
  let valNodes = match types with
    | Some types -> List.map2 (fun v ty -> wrap_value ?src ~ty v) vs types
    | None -> List.map (wrap_value ?src) vs
  in
  let types' = map_default_types types valNodes in
  { exp = Values valNodes; exp_src = src; exp_types=types' }

let cast ?src t v =
  { exp = Cast(t, v); exp_types = [t]; exp_src = src }

let exp ?src ?types exp =
  (* WARNING--- function calls may need more than 1 return type, in which
     case the default [BottomT] is wrong
  *)
  let types' = match types, exp with
    | Some ts, _ -> ts
    | None, Values vs -> List.fill Type.BottomT vs
    | _ -> [Type.BottomT]
  in
  { exp= exp; exp_types = types'; exp_src = src}

let call ?src fnId outTypes args  =
  { exp = Call(fnId, args); exp_types = outTypes; exp_src=src}

let closure fundef args = {
  closure_fn = fundef.fn_id;
  closure_args = args;
  closure_arg_types = List.map (fun v -> v.value_type) args;

}

(****************************************************************)
(*                Statement Helpers                             *)
(****************************************************************)

let stmt ?src ?(id=StmtId.gen()) stmt =
  { stmt = stmt; stmt_src = src; stmt_id = id }

let set ?src ids rhs =
  { stmt = Set(ids, rhs);
    stmt_src = src;
    stmt_id = StmtId.gen()
  }

let setidx ?src lhs indices rhs =
  { stmt = SSA.SetIdx(lhs, indices, rhs);
    stmt_src = src;
    stmt_id = StmtId.gen()
  }

(**********************************************************************)
(*           DSL for more compactly building small SSA functions      *)
(**********************************************************************)

let (<--) xs y = set (List.map get_id xs) y
let (@@) fn args = app fn args
let scalar_op (x : Prim.scalar_op)  = op (Prim.ScalarOp x)
let array_op (x : Prim.array_op) = op (Prim.ArrayOp x)
let impure_op (x : Prim.impure_op)  = op (Prim.ImpureOp x)

let print = impure_op Prim.Print

let plus = scalar_op Prim.Add
let minus = scalar_op Prim.Sub
let mul = scalar_op Prim.Mult
let div = scalar_op Prim.Div

let lt = scalar_op Prim.Lt
let lte = scalar_op Prim.Lte
let eq = scalar_op Prim.Eq

let zero = num (ParNum.Int32 0l)
let one = num (ParNum.Int32 1l)
let neg_one = num (ParNum.Int32 (-1l))
let trueVal = num (ParNum.Bool true)
let falseVal = num (ParNum.Bool false)

let inf = num (ParNum.Inf Type.Float32T)
let neginf = num (ParNum.NegInf Type.Float32T)

let select = op (Prim.ScalarOp Prim.Select)

let reduce = op  (Prim.Adverb Prim.Reduce)
let map = op (Prim.Adverb Prim.Map)
let allPairs = op (Prim.Adverb Prim.AllPairs)

(*let where = op (Prim.ArrayOp Prim.Where)*)
let index = op (Prim.ArrayOp Prim.Index)
(*let til = op (Prim.ArrayOp Prim.Til)*)
let find = op (Prim.ArrayOp Prim.Find)
let dimsize = op (Prim.ArrayOp Prim.DimSize)


let value x = exp $ Values [x]
let values xs = exp $ Values xs

let len x = dimsize @@ [x; zero]
let incr (x:ID.t) (y:value_node) = set [x] (plus @@ [y;one])
let set_int (x:ID.t) (y:Int32.t) =
  set [x] (vals_exp [Num (ParNum.Int32 y)])

(****************************************************************)
(*                 Phi-Node Helpers                             *)
(****************************************************************)


(* get the ids from a list of variable value nodes *)
let get_ids vars = List.map get_id vars

let get_fn_id valNode = match valNode.value with
  | GlobalFn fnId -> fnId
  | other -> failwith $
      Printf.sprintf
        "[SSA->get_fn_id] expected global function, received %s"
        (value_to_str other)

let get_fn_ids valNodes = List.map get_fn_id valNodes
let empty_stmt = set [] (vals_exp [])

let is_empty_stmt stmtNode =
  match stmtNode.stmt with
    | Set ([], {exp=Values[]})->true
    | _ -> false

let rec types_of_value_nodes = function
  | [] -> []
  | vNode::rest -> vNode.value_type :: (types_of_value_nodes rest)

(* special case for creating function with 1 input, 1 output *)
let untyped_fn1_builder constructor =
  fn_builder
    ?name:None
    ~input_types:[Type.BottomT]
    ~output_types:[Type.BottomT]
    ~local_types:[]
    (function [x], [y], [] -> constructor x y | _ -> assert false)

(* 2 inputs, 1 output, 0 locals *)
let untyped_fn2_builder constructor =
  fn_builder
    ?name:None
    ~input_types:[Type.BottomT]
    ~output_types:[Type.BottomT]
    ~local_types:[]
    (function [x;y], [z], [] -> constructor x y z | _ -> assert false)
