(* pp: -parser o pa_macro.cmo *)

open Base 



type cuda_info = ThreadIdx | BlockIdx | BlockDim | GridDim   
type coord = X | Y | Z

type exp = 
  | Var of ID.t
  | Idx of exp_node list * exp_node
  | Op of Prim.scalar_op * Type.elt * exp_node list
  | Select of Type.t * exp_node * exp_node * exp_node
  | Const of ParNum.t
  | Cast of Type.t * exp_node
  | DimSize of exp_node * exp_node
  | CudaInfo of cuda_info * coord

and exp_node = {
  exp : exp;
  exp_type : Type.t;
}
and stmt =
  | If of exp_node * block * block
  | While of exp_node * block
  | Set of ID.t * exp_node 
  | SetIdx of ID.t * exp_node list * exp_node
  | SyncThreads
  | Comment of string
  (* used to plug one function into another, shouldn't exist in final code *) 
  | SPLICE 
and block = stmt list
   
and array_storage = 
  | Global
  | Private
  | Shared
  | Slice
and fn = {
  input_ids : ID.t array;
  input_id_set : ID.t MutableSet.t; 
  input_types : Type.t array;
          
  output_ids : ID.t array; 
  output_id_set : ID.t MutableSet.t; 
  output_types : Type.t array;
  
  local_id_set : ID.t MutableSet.t; 
  
  types : (ID.t, Type.t) Hashtbl.t;
  
  (* only stores IDs/sizes belonging to arrays *)  
  sizes:  (ID.t, exp_node list) Hashtbl.t; 
  array_storage : (ID.t, array_storage) Hashtbl.t;
   
  body : block;
}


(* PRETTY PRINTING *) 
open Printf 

let coord_to_str = function 
  | X -> "x" | Y -> "y" | Z -> "z"

let rec exp_node_to_str e  = exp_to_str e.exp 
and exp_to_str = function 
  | Var id -> ID.to_str id  
  | Idx (e1, e2) -> sprintf "%s[%s]" (exp_node_to_str e1) (exp_node_to_str e2) 
  | Op (op, argT, args) -> 
    sprintf "%s:%s (%s)" 
      (Prim.scalar_op_to_str op)
      (Type.to_str argT) 
      (exp_node_list_to_str args)
  | Select (t, cond, trueVal, falseVal) -> 
      sprintf "select:%s(%s, %s, %s)" 
        (Type.to_str t)
        (exp_node_to_str cond)
        (exp_node_to_str trueVal)
        (exp_node_to_str falseVal)
  | Const n -> ParNum.num_to_str n 
  | Cast (tNew, e) -> 
      let tOld = e.exp_type in 
      sprintf "cast %s->%s (%s)" 
        (Type.to_str tOld) 
        (Type.to_str tNew) 
        (exp_node_to_str e)
  | DimSize (k, e) -> 
        sprintf "dimsize(%s, %s)" (exp_node_to_str e) (exp_node_to_str k) 
  | ThreadIdx c -> sprintf "threadidx.%s" (coord_to_str c)
  | BlockIdx c -> sprintf "blockidx.%s" (coord_to_str c)
  | BlockDim c -> sprintf "blockdim.%s" (coord_to_str c)
  | GridDim c -> sprintf "griddim.%s" (coord_to_str c)
  

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
  | SPLICE -> spaces ^ "SPLICE"
and block_to_str ?(spaces="") stmts = 
  String.concat "\n" (List.map (stmt_to_str ~spaces) stmts)
and exp_node_list_to_str exps = 
  String.concat ", " (List.map exp_node_to_str exps)
and array_storage_to_str = function 
  | Global -> "global"
  | Private -> "private"
  | Shared -> "shared"
  | Slice -> "slice"
and shared_to_str fn = 
  let s = ref "" in
  let extend_string id = 
    if Hashtbl.mem fn.array_storage id  && 
       not $ MutableSet.mem fn.input_id_set id then
        let currStr = 
          Printf.sprintf "  %s %s :: [%s]"
            (array_storage_to_str $ Hashtbl.find fn.array_storage id)   
            (ID.to_str id)
            (exp_node_list_to_str $ Hashtbl.find fn.sizes id)
         in s := !s ^"\n" ^ currStr 
  in
  if not $ MutableSet.is_empty fn.local_id_set then s := !s ^ "\n"; 
  MutableSet.iter  extend_string fn.local_id_set;
  if not $ MutableSet.is_empty fn.local_id_set then s := !s ^ "\n";  
  !s 
let fn_to_str fn =
  let id_to_str id  = 
    let t = Hashtbl.find fn.types id in 
    ID.to_str id ^ " : " ^ (Type.to_str t)
  in 
  let inputs = List.map id_to_str (Array.to_list fn.input_ids)  in 
  let outputs = List.map id_to_str (Array.to_list fn.output_ids) in 
  let bodyStr = block_to_str  fn.body in 
  sprintf "fn (%s) -> (%s) = {%s\n%s\n}"
    (String.concat ", " inputs) 
    (String.concat ", " outputs)
    (shared_to_str fn)  
    bodyStr  
              