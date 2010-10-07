open Base
open SourceInfo 
include SSA_Gates 


(* eventually also need a gate for while loops with merges for variables in 
   the loop and merges for after the loop *)

module StmtId = UID.Make(struct let to_str x  = "stmt" ^ (string_of_int x) end)
 

type stmt = 
  | Set of ID.t list * exp_node 
  | Ignore of exp_node
  | SetIdx of ID.t * (value_node list) * value_node
  | If of value_node * block * block * SSA_Gates.if_gate
  (*| WhileLoop of value * block * SSA_Gates.while_gate*)    
and stmt_node = { 
    stmt: stmt;
    stmt_src: source_info option;
    stmt_id : StmtId.t;  
}
and block = stmt_node list 
and  exp = 
       
  | App of  value_node * value_node list
  | ArrayIndex of value_node * value_node list
  | Arr of value_node list
  | Cast of DynType.t * value_node  
  | Values of value_node list 
and exp_node = { 
  exp: exp; 
  exp_src : source_info option;
  (* because a function applicatin might return multiple values,*)
  (* expressions have multiple types *)  
  exp_types : DynType.t list; 
} 
and value_node = { 
  value_type : DynType.t;
  value_src : source_info option; 
  value : value 
}  
and value = 
  | Var of ID.t 
  | Num of PQNum.num 
  | Str of string
  | Sym of string
  | Unit
  | Prim of Prim.prim
  | Lam of fundef
and fundef = {
  input_ids:ID.t list;
  output_ids: ID.t list; 
  body: block;
  tenv : (ID.t, DynType.t) PMap.t; 
  fun_type : DynType.t  
}
let is_simple_exp = function 
  | App _ -> false
  | _ -> true 


open Printf 

let rec id_list_to_str = function 
  | [] -> ""
  | [id] -> ID.to_str id 
  | id::rest -> (ID.to_str id) ^ ", " ^ (id_list_to_str rest) 

let rec typed_id_to_str tenv id = 
  if PMap.mem id tenv then 
    sprintf "%s : %s" (ID.to_str id) (DynType.to_str (PMap.find id tenv))
  else sprintf "%s" (ID.to_str id) 

let rec typed_id_list_to_str tenv = function 
  | [] -> ""
  | [id] -> typed_id_to_str tenv id 
  | id::rest -> 
    sprintf "%s, %s" (typed_id_to_str tenv id) (typed_id_list_to_str tenv rest)
  
        
let rec block_to_str ?(space="") ?(tenv=PMap.empty) block = 
    String.concat "\n" (List.map (node_to_str ~space ~tenv) block)
and node_to_str ?(space="") ?(tenv=PMap.empty) stmtNode = 
  let str = match stmtNode.stmt with 
  | Set (ids, rhs) -> 
    sprintf "%s = %s " (typed_id_list_to_str tenv ids) (exp_to_str rhs)
  | Ignore effect -> sprintf "ignore %s " (exp_to_str effect) 
  | SetIdx _ -> "<set-idx>" 
  | If _ -> "if"
  in space ^ str
and exp_to_str expNode = 
  match expNode.exp with  
  | Values vs -> value_nodes_to_str vs 
  | App ({value=Prim op}, args) ->    
    sprintf "%s(%s)" (Prim.prim_to_str op) (value_node_list_to_str args)
  | App (fn, args) -> 
    sprintf "%s(%s)" (value_node_to_str fn)  (value_node_list_to_str args)
  | ArrayIndex (array, indices) ->  
      sprintf "%s[%s]" (value_node_to_str array) 
       (value_node_list_to_str ~sep:";" indices)
  | Arr elts -> "[" ^ (value_node_list_to_str ~sep:";" elts) ^ "]" 
  (*| TupleProj(idx, tuple) -> 
      sprintf "proj(%s, %s)" (value_to_str idx) (value_to_str tuple)*)
  
  (*| Tuple elts -> "(" ^ (value_list_to_str elts)  ^ ")"*)
and value_node_to_str vNode =
  let valStr = value_to_str vNode.value in  
  if vNode.value_type <> DynType.BottomT then 
    sprintf "%s : %s" valStr (DynType.to_str vNode.value_type)
  else valStr
and value_nodes_to_str = function 
  | [] -> ""
  | [v] -> value_node_to_str v
  | v::vs -> (value_node_to_str v) ^ "; " ^ (value_nodes_to_str vs)  
and value_to_str = function 
  | Var id -> ID.to_str id 
  | Num n -> PQNum.num_to_str n 
  | Str s -> "\""^s ^"\""
  | Sym s -> "`" ^ s
  | Unit -> "()"
  | Prim p -> "PRIM(" ^ (Prim.prim_to_str p) ^ ")"
  | Lam fundef -> 
    Format.sprintf "fun input:[%s] output:[%s] { \n @[<hov 2> %s @] \n}" 
      (String.concat ", " (List.map ID.to_str fundef.input_ids))
      (String.concat ", " (List.map ID.to_str fundef.output_ids))
      (block_to_str fundef.body)
  
and value_node_list_to_str ?(sep=",") vs = 
  String.concat (sep^" ") (List.map value_node_to_str vs)
  
and value_list_to_str ?(sep=",") vs = 
  String.concat (sep^" ") (List.map value_to_str vs)

let fundef_to_str fundef = 
  sprintf "(%s) = fn (%s) -> \n %s" 
    (typed_id_list_to_str fundef.tenv fundef.output_ids)  
    (typed_id_list_to_str fundef.tenv fundef.input_ids)
    (block_to_str ~space:"\t" ~tenv:fundef.tenv fundef.body)
  

(***
     helpers for statements 
 ***) 

let mk_stmt ?src ?(id=StmtId.gen()) stmt = 
  { stmt = stmt; stmt_src = src; stmt_id = id }   

let mk_set ?(src=None) ids rhs = 
  { stmt = Set(ids, rhs); 
    stmt_src = src; 
    stmt_id = StmtId.gen() 
  }

let mk_if ?(src=None) condVal tBlock fBlock gate = 
  { stmt = If(condVal, tBlock, fBlock, gate); 
    stmt_src=src; 
    stmt_id = StmtId.gen() 
  } 

(* get the id of a variable value node *) 
let get_id valNode = match valNode.value with 
  | Var id -> id 
  | _ -> failwith "variable expected"
  
(* get the ids from a list of variable value nodes *) 
let get_ids vars = List.map get_id vars 


(***
    helpers for values 
 ***)

let mk_val ?(src=None) ?(ty=DynType.BottomT) (v:value) : value_node = 
  { value = v; value_src = src; value_type = ty }

let mk_var ?(src=None) ?(ty=DynType.BottomT) (id:ID.t) : value_node = 
  { value = Var id; value_src = src; value_type = ty }    

(*** 
    helpers for expressions 
 ***) 

let map_default_types optTypes values = 
  match optTypes with 
    | None -> List.map (fun vNode -> vNode.value_type) values 
    | Some ts -> ts

let mk_app ?(src=None) ?types fn args =
  let types' = map_default_types types args in  
  { exp=App(fn,args); exp_src = src; exp_types = types' }  

let mk_arr ?(src=None) ?types elts =
  let types' = map_default_types  types elts in  
  { exp=Arr elts; exp_src=src; exp_types = types' } 
 
let mk_val_exp ?(src=None) ?ty (v: value) =
  let ty' = match ty with 
    | None -> DynType.BottomT 
    | Some ty -> ty 
  in 
  { exp=Values [mk_val ~src v]; exp_src=src; exp_types = [ty'] } 

let mk_vals_exp ?(src=None) ?types ( vs : value list) =
  let valNodes = List.map (mk_val ~src) vs in 
  let types' = map_default_types types valNodes in 
  { exp = Values valNodes; exp_src = src; exp_types=types' } 

let mk_arr_idx ?(src=None) ?(types=[DynType.BottomT]) lhs indices =
  { exp = ArrayIndex(lhs, indices); exp_src=src; exp_types=types} 
        
let mk_cast ?(src=None)  t v = 
  { exp = Cast(t, v); exp_types = [t]; exp_src = src }      

let mk_exp ?(src=None) ?types exp =
  (* WARNING--- function calls may need more than 1 return type, in which 
     case the default [BottomT] is wrong 
  *) 
  let types' = match types, exp with 
    | Some ts, _ -> ts
    | None, Values vs -> List.fill DynType.BottomT vs
    | _ -> [DynType.BottomT] 
  in 
  { exp= exp; exp_types = types'; exp_src = src} 
