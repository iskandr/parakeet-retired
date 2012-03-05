(* pp: -parser o pa_macro.cmo *)

open Base

type cuda_info = ThreadIdx | BlockIdx | BlockDim | GridDim
type coord = X | Y | Z

type array_field =
  | ArrayData
  | ArrayShape
  | ArrayStrides
  | RangeStart
  | RangeStop
  | ShiftData
  | ShiftAmt
  | ShiftDim
  | ShiftDefault
  | RotData
  | RotDim
  | RotAmt


let fields_of_type = function
  | ImpType.ScalarT _ -> []
  | ImpType.ArrayT _ -> [ArrayData; ArrayShape; ArrayStrides]
  | ImpType.RangeT _ -> [RangeStart; RangeStop]
  | ImpType.PtrT _ -> failwith "Pointers have no fields"
  | ImpType.ExplodeT _ -> failwith "Explode not yet implemented"
  | ImpType.RotateT _ -> failwith "Rotate not yet implemented"
  | ImpType.ShiftT _ -> failwith "Shift not yet implemented"


let rec field_types = function
  | ImpType.ScalarT _ -> []
  | ImpType.ArrayT(eltT, rank) ->
    [
      ImpType.PtrT (eltT, None);
      ImpType.PtrT (Type.Int32T, Some rank);
      ImpType.PtrT (Type.Int32T, Some rank);
    ]
  | _ -> failwith "Not implemented"


let array_field_pos = function
  | ArrayData -> 0
  | ArrayShape -> 1
  | ArrayStrides -> 2
  | RangeStart -> 0
  | RangeStop -> 1
  | ShiftData -> 0
  | ShiftAmt -> 1
  | ShiftDim -> 2
  | ShiftDefault -> 3
  | RotData -> 0
  | RotDim -> 1
  | RotAmt -> 2

type value =
  | Var of ID.t
  | Const of ParNum.t
  | VecConst of ParNum.t list
  | CudaInfo of cuda_info * coord
  | Idx of value_node * value_node list
  | VecSlice of value_node * value_node list
  | Val of value_node
  | Op of ImpType.t * Prim.scalar_op * value_node list
  | Select of ImpType.t * value_node * value_node * value_node
  | Cast of ImpType.t * value_node
  | DimSize of value_node * value_node
  | FixDim of value_node * value_node * value_node
  | Slice of value_node * value_node * value_node * value_node
  | ArrayField of array_field * value_node
and value_node = {
  value : value;
  value_type : ImpType.t;
}

type value_nodes = value_node list

type stmt =
  | If of value_node * block * block
  | While of value_node * block (* test, body *)
  | Set of ID.t * value_node
  | SetIdx of value_node * value_node list * value_node
  | SetVecSlice of value_node * value_node list * value_node
  | SyncThreads
  | Comment of string
and block = stmt list

type storage =
  | Global
  | Local
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
  shapes = ID.Map.empty
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

let array_field_to_str = function
  | ArrayData -> "data"
  | ArrayShape -> "shape"
  | ArrayStrides -> "strides"
  | RangeStart -> "range_start"
  | RangeStop -> "range_stop"
  | ShiftData -> "shift_data"
  | ShiftAmt -> "shift_amt"
  | ShiftDim -> "shift_dim"
  | ShiftDefault -> "shift_default"
  | RotData -> "rot_data"
  | RotDim -> "rot_dim"
  | RotAmt -> "rot_amt"

let rec value_to_str = function
  | Var id -> ID.to_str id
  | Const n -> ParNum.to_str n
  | VecConst ns ->
    sprintf "(%s)" (String.concat ", " (List.map ParNum.to_str ns))
  | CudaInfo(cuda_info, coord) ->
    sprintf "%s.%s" (cuda_info_to_str cuda_info) (coord_to_str coord)
  | Idx (arr, args) ->
    sprintf "%s[%s]"
      (value_node_to_str arr)
      (value_nodes_to_str args)
  | VecSlice (arr, args) ->
    sprintf "vecslice(%s[%s])"
      (value_node_to_str arr)
      (value_nodes_to_str args)
  | Op (argT, op, args) ->
    sprintf "%s:%s (%s)"
      (Prim.scalar_op_to_str op)
      (ImpType.to_str argT)
      (value_nodes_to_str args)
  | Select (t, cond, trueVal, falseVal) ->
    sprintf "select:%s(%s, %s, %s)"
      (ImpType.to_str t)
      (value_node_to_str cond)
      (value_node_to_str trueVal)
      (value_node_to_str falseVal)
  | Cast (tNew, v) ->
    sprintf "cast %s->%s (%s)"
      (ImpType.to_str v.value_type) (ImpType.to_str tNew)
      (value_node_to_str v)
  | DimSize (arr, idx) ->
    sprintf "dimsize(%s, %s)" (value_node_to_str arr) (value_node_to_str idx)
  | FixDim (arr, dim, idx) ->
    sprintf "fixdim(%s, dim=%s, idx=%s)"
      (value_node_to_str arr)
      (value_node_to_str dim)
      (value_node_to_str idx)
  | Slice (arr, dim, start, stop) ->
    sprintf "slice(%s, dim=%s, start=%s, stop=%s)"
      (value_node_to_str arr)
      (value_node_to_str dim)
      (value_node_to_str start)
      (value_node_to_str stop)
  | ArrayField(field, v) ->
     sprintf "field(%s, %s)"
       (array_field_to_str field)
       (value_node_to_str v)

and value_node_to_str {value} = value_to_str value
and value_nodes_to_str vNodes =
  String.concat ", " (List.map value_node_to_str vNodes)

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
	    (value_node_to_str cond)
	    tStr
	    fStr
  | While (cond, body) ->
    let bodyStr =
      if body <> [] then "\n" ^ (block_to_str ~spaces:(spaces ^ "  ") body)
      else ""
    in
    let condStr = (value_node_to_str cond) in
    sprintf "%s while(%s) { %s }" spaces condStr bodyStr
  | Set (id, rhs) ->
    sprintf "%s %s = %s" spaces (ID.to_str id) (value_node_to_str rhs)
  | SetIdx (lhs, indices, rhs) ->
    let lhsStr = value_node_to_str lhs in
    let idxStr = value_nodes_to_str indices in
    let rhsStr = value_node_to_str rhs in
	  sprintf "%s %s[%s] = %s" spaces lhsStr idxStr rhsStr
  | SetVecSlice (lhs, indices, rhs) ->
    let lhsStr = value_node_to_str lhs in
    let idxStr = value_nodes_to_str indices in
    let rhsStr = value_node_to_str rhs in
    sprintf "%s vecslice(%s[%s]) = %s" spaces lhsStr idxStr rhsStr
  | SyncThreads -> spaces ^ "syncthreads"
  | Comment s -> spaces ^ "// " ^ s
  (* used to plug one function into another, shouldn't exist in final code *)
  (*| SPLICE -> spaces ^ "SPLICE"*)
and block_to_str ?(spaces="") stmts =
  String.concat "\n" (List.map (stmt_to_str ~spaces) stmts)

let array_storage_to_str = function
  | Global -> "global"
  | Local -> "local"
  | Alias -> "alias"
  | CudaShared -> "shared"

let fn_to_str fn =
  let id_to_str id =
    ID.to_str id ^ " : " ^ (ImpType.to_str (get_var_type fn id))
  in
  let inputs = List.map id_to_str fn.input_ids  in
  let outputs = List.map id_to_str  fn.output_ids in
  let decl_str localId =  " local " ^ (id_to_str localId) in
  let localDeclStr = String.concat "\n" (List.map decl_str fn.local_ids) in
  let shape_str outputId =
    Printf.sprintf " shape(%s) = %s"
    (ID.to_str outputId)
    (SymbolicShape.to_str (ID.Map.find outputId fn.shapes))
  in
  let outputShapeStr = String.concat "\n" (List.map shape_str fn.output_ids) in
  sprintf "fn (%s) -> (%s) = {\n%s%s%s\n}"
    (String.concat ", " inputs)
    (String.concat ", " outputs)
    (if String.length localDeclStr > 0 then localDeclStr ^ "\n" else "")
    (if String.length outputShapeStr > 0 then outputShapeStr ^"\n\n" else "")
    (block_to_str fn.body)

let rec always_const {value} = match value with
  | CudaInfo _
  | Const _
  | VecConst _
  | DimSize _ -> true
  | Cast (_, arg) -> always_const arg
  | Select (_, pred, arg1, arg2) ->
    always_const pred && always_const arg1 && always_const arg2
  | Op (_, _, args) -> List.for_all always_const args
  | _ -> false

