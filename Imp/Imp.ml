(* pp: -parser o pa_macro.cmo *)

open Base

type cuda_info = ThreadIdx | BlockIdx | BlockDim | GridDim
type coord = X | Y | Z

type array_field =
  | RangeStart
  | RangeStop
  | ShiftData
  | ShiftAmt
  | ShiftDim
  | ShiftDefault
  | RotData
  | RotDim
  | RotAmt
  | SliceData
  | SliceDim
  | SliceStart
  | SliceStop
  | FrozenData
  | FrozenDim
  | FrozenIdx

type value =
  | Var of ID.t
  | Const of ParNum.t
  | CudaInfo of cuda_info * coord

and value_node = {
  value : value;
  value_type : ImpType.t;
}

type exp =
  | Val of value_node
  | Op of  Type.elt_t * Prim.scalar_op * value_node list
  | Select of ImpType.t * value_node * value_node * value_node
  | Cast of ImpType.t * value_node
  | Idx of value_node * value_node list
  | DimSize of value_node * value_node
  | FreezeDim of value_node * value_node * value_node
  | ArrayField of array_field * value_node

and exp_node = {
  exp : exp;
  exp_type : ImpType.t;
}

type stmt =
  | If of value_node * block * block
  | While of exp_node * block (* test, body *)
  | Set of ID.t * exp_node
  | SetIdx of value_node * value_node list * value_node
  | SyncThreads
  | Comment of string
  (* used to plug one function into another, shouldn't exist in final code *)
  (*
  | SPLICE
  *)
and block = stmt list

type storage =
  | Stack
  | HeapAlloc
  | Alias
  | CudaShared

type fn = {
  id : FnId.t;
  input_ids : ID.t list;
  output_ids : ID.t list;
  local_ids : ID.t list;

  storage : storage ID.Map.t;
  types : ImpType.t ID.Map.t;
  shapes : SymbolicShape.t ID.Map.t;

  body : block;
}

let empty_fn = {
  id = FnId.gen();
  input_ids = [];
  output_ids = [];
  body = [];
  local_ids = [];
  storage = ID.Map.empty;
  types = ID.Map.empty;
  shapes=ID.Map.empty
}



let get_var_type (fn:fn) (id:ID.t) =
  match ID.Map.find_option id fn.types with
  | None -> failwith $ "[Imp->get_var_type] Variable " ^ (ID.to_str id) ^
                       "doesn't exist"
  | Some var_type -> var_type

let get_var_storage (fn:fn) (id:ID.t) =
  match ID.Map.find_option id fn.storage with
  | None -> failwith $ "[Imp->get_var_storage] Variable " ^ (ID.to_str id) ^
                       "doesn't exist"
  | Some storage -> storage

let get_var_shape (fn:fn) (id:ID.t) =
  match ID.Map.find_option id fn.shapes with
  | None -> failwith $ "[Imp->get_var_shape] Variable " ^ (ID.to_str id) ^
                       "doesn't exist"
  | Some symbolic_shape -> symbolic_shape

let input_types fn = List.map (get_var_type fn) fn.input_ids
let output_types fn = List.map (get_var_type fn) fn.output_ids
let local_types fn = List.map (get_var_type fn) fn.local_ids

let output_shapes fn  = List.map (get_var_shape fn) fn.output_ids
let local_shapes fn = List.map (get_var_shape fn) fn.input_ids

let value_type {value_type} = value_type
let value_types valNodes = List.map value_type valNodes

(* PRETTY PRINTING *)
open Printf

let coord_to_str = function
  | X -> "x" | Y -> "y" | Z -> "z"

let cuda_info_to_str = function
	| ThreadIdx -> "threadidx"
  | BlockIdx -> "blockidx"
  | BlockDim -> "blockdim"
  | GridDim -> "griddim"

let val_to_str = function
  | Var id -> ID.to_str id
  | Const n -> ParNum.to_str n
  | CudaInfo(cuda_info, coord) ->
     sprintf "%s.%s" (cuda_info_to_str cuda_info) (coord_to_str coord)

let val_node_to_str {value} = val_to_str value

let val_node_list_to_str exps =
  String.concat ", " (List.map val_node_to_str exps)

let rec exp_node_to_str e  = exp_to_str e.exp
and exp_to_str = function
  | Val v -> val_node_to_str v
  | Idx (arr, args) ->
    sprintf "%s[%s]"
      (val_node_to_str arr)
      (val_node_list_to_str args)
  | Op (argT, op, args) ->
    sprintf "%s:%s (%s)"
      (Prim.scalar_op_to_str op)
      (Type.elt_to_str argT)
      (val_node_list_to_str args)
  | Select (t, cond, trueVal, falseVal) ->
      sprintf "select:%s(%s, %s, %s)"
        (ImpType.to_str t)
        (val_node_to_str cond)
        (val_node_to_str trueVal)
        (val_node_to_str falseVal)
  | Cast (tNew, v) ->
      sprintf "cast %s->%s (%s)"
        (ImpType.to_str  v.value_type)
        (ImpType.to_str tNew)
        (val_node_to_str v)
  | DimSize (k, e) ->
      sprintf "dimsize(%s, %s)" (val_node_to_str e) (val_node_to_str k)

let rec stmt_to_str ?(spaces="") = function
  | If (cond, tBlock, fBlock) ->
	  let tStr =
	    if List.length tBlock > 1 then
	      Printf.sprintf " then {\n%s\n%s }\n%s"
	        (block_to_str ~spaces:(spaces ^ "  ") tBlock)
	        spaces
	        spaces
	    else Printf.sprintf " then { %s } " (block_to_str tBlock)
	  in
	  let fStr =
	    if List.length fBlock > 1 then
	      Printf.sprintf "\n%selse {\n%s\n%s }"
	        spaces
	        (block_to_str ~spaces:(spaces ^ "  ") fBlock)
	        spaces
	    else Printf.sprintf " else { %s }" (block_to_str fBlock)
	  in
	  sprintf "%s if (%s)%s%s "
	    spaces
	    (val_node_to_str cond)
	    tStr
	    fStr
  | While (cond, body) ->
      let bodyStr =
        if body <> [] then "\n" ^ (block_to_str ~spaces:(spaces ^ "  ") body)
        else ""
      in
      let condStr = (exp_node_to_str cond) in
      sprintf "%s while(%s) { %s }" spaces condStr bodyStr
  | Set (id, rhs) ->
    sprintf "%s %s = %s" spaces (ID.to_str id) (exp_node_to_str rhs)
  | SetIdx (lhs, indices, rhs) ->
    let lhsStr = val_node_to_str lhs in
    let idxStr = val_node_list_to_str indices in
    let rhsStr = val_node_to_str rhs in
	  sprintf "%s %s[%s] = %s" spaces lhsStr idxStr rhsStr
  | SyncThreads -> spaces ^ "syncthreads"
  | Comment s -> spaces ^ "// " ^ s
  (* used to plug one function into another, shouldn't exist in final code *)
  (*| SPLICE -> spaces ^ "SPLICE"*)
and block_to_str ?(spaces="") stmts =
  String.concat "\n" (List.map (stmt_to_str ~spaces) stmts)

let array_storage_to_str = function
  | Stack -> "stack"
  | HeapAlloc -> "heap"
  | Alias -> "alias"
  | CudaShared -> "shared"

let fn_to_str fn =
  let id_to_str id =
    ID.to_str id ^ " : " ^ (ImpType.to_str (get_var_type fn id))
  in
  let inputs = List.map id_to_str fn.input_ids  in
  let outputs = List.map id_to_str  fn.output_ids in
  let decl_str id =  "local " ^ (id_to_str id) in
  let localDeclStr = String.concat "\n" (List.map decl_str fn.local_ids) in
  sprintf "fn (%s) -> (%s) = { \n%s%s\n}"
    (String.concat ", " inputs)
    (String.concat ", " outputs)
    (if String.length localDeclStr > 0 then localDeclStr ^ "\n" else "")
    (block_to_str  fn.body)

let always_const_val {value} = match value with
  | CudaInfo _
  | Const _ -> true
  | _ -> false

let rec always_const_exp {exp} = match exp with
  | DimSize _ -> true
  | Val v -> always_const_val v
  | Cast (_, arg) -> always_const_val arg
  | Select (_, pred, arg1, arg2) -> always_const_val pred && always_const_val arg1 && always_const_val arg2
  | Op (_, _, args) -> List.for_all always_const_val args
  | _ -> false
