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
  | SliceDim
  | SliceStart
  | SliceStop
  | FrozenDim
  | FrozenIdx
 
type exp = 
  | Var of ID.t
  | Const of ParNum.t
  | Op of  Type.elt_t * Prim.scalar_op * exp_node list
  | Select of ImpType.t * exp_node * exp_node * exp_node
  | Cast of ImpType.t * exp_node
  | Idx of exp_node * exp_node list
  | DimSize of exp_node * exp_node 
  | FreezeDim of exp_node * exp_node * exp_node 
  | ArrayField of array_field * exp_node
  | CudaInfo of cuda_info * coord

and exp_node = {
  exp : exp;
  exp_type : ImpType.t;
}

type stmt =
  | If of exp_node * block * block
  | While of exp_node * block
  | Set of ID.t * exp_node 
  | SetIdx of ID.t * exp_node list * exp_node
  | SyncThreads
  | Comment of string
  (* used to plug one function into another, shouldn't exist in final code *) 
  (*
  | SPLICE
  *) 
and block = stmt list
   
type storage = 
  | Global
  | Private
  | Shared
  | Alias


type fn = {
  input_ids : ID.t list;
  output_ids : ID.t list; 
  local_ids : ID.t list; 
  
  storage : storage ID.Map.t;
  types : ImpType.t ID.Map.t;
  shapes : exp_node list ID.Map.t;
  
  body : block;
}
let get_var_type (fn:fn) (id:ID.t) = 
	match ID.Map.find_option id fn.types with 
		| None -> failwith $ "[Imp->get_var_type] Variable " ^ (ID.to_str id) ^ "doesn't exist"
		| Some var_type -> var_type 

let get_var_storage (fn:fn) (id:ID.t) =
	match ID.Map.find_option id fn.storage with 
		| None -> failwith $ "[Imp->get_var_storage] Variable " ^ (ID.to_str id) ^ "doesn't exist"
		| Some storage -> storage

let get_var_shape (fn:fn) (id:ID.t) = 
	match ID.Map.find_option id fn.shapes  with 
		| None -> failwith $ "[Imp->get_var_shape] Variable " ^ (ID.to_str id) ^ "doesn't exist"
		| Some symbolic_shape -> symbolic_shape
	 
(* PRETTY PRINTING *) 
open Printf 

let coord_to_str = function 
  | X -> "x" | Y -> "y" | Z -> "z"

let cuda_info_to_str = function 
	| ThreadIdx -> "threadidx" 
  | BlockIdx -> "blockidx"
  | BlockDim -> "blockdim"
  | GridDim -> "griddim"


let rec exp_node_to_str e  = exp_to_str e.exp 
and exp_to_str = function 
  | Var id -> ID.to_str id  
  | Idx (arr, args) -> sprintf "%s[%s]" (exp_node_to_str arr) (exp_node_list_to_str args) 
  | Op (argT, op, args) -> 
    sprintf "%s:%s (%s)" 
      (Prim.scalar_op_to_str op)
      (Type.elt_to_str argT) 
      (exp_node_list_to_str args)
  | Select (t, cond, trueVal, falseVal) -> 
      sprintf "select:%s(%s, %s, %s)" 
        (ImpType.to_str t)
        (exp_node_to_str cond)
        (exp_node_to_str trueVal)
        (exp_node_to_str falseVal)
  | Const n -> ParNum.to_str n 
  | Cast (tNew, e) -> 
      sprintf "cast %s->%s (%s)" 
        (ImpType.to_str  e.exp_type) 
        (ImpType.to_str tNew) 
        (exp_node_to_str e)
  | DimSize (k, e) -> 
        sprintf "dimsize(%s, %s)" (exp_node_to_str e) (exp_node_to_str k)
	| CudaInfo(cuda_info, coord) -> 
				sprintf "%s.%s" (cuda_info_to_str cuda_info) (coord_to_str coord)

and stmt_to_str ?(spaces="") = function 
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
        (exp_node_to_str cond)
        tStr 
        fStr
  | While (cond, body) ->
      let bodyStr = 
        if body <> [] then "\n" ^ (block_to_str ~spaces:(spaces ^ "  ") body)
        else ""
      in  
      sprintf "%s while(%s) { %s }" spaces (exp_node_to_str cond) bodyStr 
        
  | Set (id, rhs) -> 
      sprintf "%s %s = %s" spaces (ID.to_str id) (exp_node_to_str rhs)  
  | SetIdx (id, indices, rhs) -> 
      sprintf "%s %s[%s] = %s"
        spaces 
        (ID.to_str id) 
        (exp_node_list_to_str indices) 
        (exp_node_to_str rhs)
  | SyncThreads -> spaces ^ "syncthreads"
  | Comment s -> spaces ^ "// " ^ s
  (* used to plug one function into another, shouldn't exist in final code *) 
  (*| SPLICE -> spaces ^ "SPLICE"*)
and block_to_str ?(spaces="") stmts = 
  String.concat "\n" (List.map (stmt_to_str ~spaces) stmts)
and exp_node_list_to_str exps = 
  String.concat ", " (List.map exp_node_to_str exps)
and array_storage_to_str = function 
  | Global -> "global"
  | Private -> "private"
  | Shared -> "shared"
  | Alias -> "alias"
 
	
let fn_to_str fn =
  let id_to_str id  = 
    ID.to_str id ^ " : " ^ (ImpType.to_str (get_var_type fn id))
  in 
  let inputs = List.map id_to_str fn.input_ids  in 
  let outputs = List.map id_to_str  fn.output_ids in
	let locals = List.map id_to_str fn.local_ids in  
  sprintf "fn (%s) -> (%s) = {%s\n%s\n}"
    (String.concat ", " inputs) 
    (String.concat ", " outputs)
		(String.concat ", " locals)
    (block_to_str  fn.body)
              
let rec always_const {exp} = match exp with
  | CudaInfo _ 
  | DimSize _  
  | Const _ -> true
  | Cast (_, arg) -> always_const arg 
  | Select (_, pred, arg1, arg2) -> always_const pred && always_const arg1 && always_const arg2  
  | Op (_, _, args) -> List.for_all always_const args 
  | _ -> false 
  