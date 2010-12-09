(* pp: -parser o pa_macro.cmo *)

open Base
open SourceInfo 

type tenv = DynType.t ID.Map.t 

 (* this IF generates the following set of identifiers, 
     whose values might be taken from either branch 
  *)  
 

type if_gate = { 
  if_output_ids : ID.t list;
  true_ids : ID.t list; 
  false_ids : ID.t list 
}

type loop_gate = { 
  (* what variables visible after this loop are generated, and
     from which internal var do they get their value?  
  *)
  loop_outputs : ID.t list; 
  
  (* what variables are assigned in the body of this loop? *) 
  loop_local_defs : ID.t list;
  
  (* every loop local variable gets its value either from above the 
     loop on the first iteration, or from a further loop variable on 
     a repeat iteration  
  *) 
  loop_header_map : (ID.t * ID.t) ID.Map.t;
  (* every loop output can either come a loop variable or from some
     variable preceding the loop (including, presumably, undefined)
  *)
  loop_output_map : (ID.t * ID.t) ID.Map.t;  
}


type ('a, 'b) stmt = 
  | Set of ID.t list * exp_node 
  | SetIdx of ID.t * value_nodes * value_node
  | If of value_node * block * block * if_gate
  | WhileLoop of block * ID.t * block * loop_gate  
and stmt_node = { 
    stmt: stmt;
    stmt_src: source_info option;
    stmt_id : StmtId.t;  
}
and block = stmt_node array
and  exp = 
  | App of  value_node * value_nodes
  | ArrayIndex of value_node * value_nodes
  | Arr of value_nodes
  | Values of value_nodes
  (* nodes below are only used after type specialization *) 
  | Cast of DynType.t * value_node  
  | Call of typed_fn * value_nodes 
  | PrimApp of typed_prim * value_nodes  
  | Map of closure * value_nodes
  | Reduce of closure * closure * value_nodes   
  | Scan of closure * closure * value_nodes 
and typed_fn = { 
  fn_id : FnId.t; 
  fn_input_types : DynType.t list; 
  fn_output_types : DynType.t list;   
} 
and typed_prim = { 
  prim_input_types : DynType.t list; 
  prim_output_types : DynType.t list; 
  prim: Prim.prim; 
} 
and closure = {   
  closure_fn: FnId.t; 
  closure_args: value_node list; 
  closure_arg_types: DynType.t list; 
  closure_input_types:DynType.t list; 
  closure_output_types: DynType.t list 
} 
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
and value_nodes = value_node list   
and value = 
  | Var of ID.t
  | GlobalFn of FnId.t  
  | Num of PQNum.num 
  | Str of string
  | Sym of string
  | Unit
  | Prim of Prim.prim
  | Lam of fundef
  (* place holder for initial values of reductions, 
     which for now are not supported but still need to 
     be given sensible types
  *) 
  | Stream of value_node * DynType.t  
and fundef = {
  body: block;
  tenv : tenv;
  input_ids:ID.t list;
  output_ids: ID.t list; 
  fundef_type : DynType.t; 
  fundef_id : FnId.t; 
}


let is_simple_exp = function
  | Call _ 
  | PrimApp _ 
  | Map _ 
  | Reduce _ 
  | Scan _  
  | App _ -> false
  | _ -> true 



open Printf 

let rec id_list_to_str = function 
  | [] -> ""
  | [id] -> ID.to_str id 
  | id::rest -> (ID.to_str id) ^ ", " ^ (id_list_to_str rest) 

let rec typed_id_to_str tenv id = 
  if ID.Map.mem id tenv then 
    sprintf "%s : %s" (ID.to_str id) (DynType.to_str (ID.Map.find id tenv))
  else sprintf "%s" (ID.to_str id) 

let rec typed_id_list_to_str tenv = function 
  | [] -> ""
  | [id] -> typed_id_to_str tenv id 
  | id::rest -> 
    sprintf "%s, %s" (typed_id_to_str tenv id) (typed_id_list_to_str tenv rest)
  
        
let rec block_to_str ?(space="") ?(tenv=ID.Map.empty) block = 
    String.concat "\n" 
    (Array.to_list 
      (Array.map (stmt_node_to_str ~space ~tenv) block))
and stmt_node_to_str ?(space="") ?(tenv=ID.Map.empty) stmtNode = 
  let str = match stmtNode.stmt with 
  | Set (ids, rhs) -> 
    sprintf "%s = %s " (typed_id_list_to_str tenv ids) (exp_to_str rhs)
  | SetIdx _ -> "<set-idx>" 
  | If _ -> "if ???"
  | WhileLoop _ -> "while ???"
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
  | Cast(t, v) -> 
      sprintf "cast<%s>(%s)" (DynType.to_str t) (value_node_to_str v) 
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
  | GlobalFn fnId -> FnId.to_str fnId 
  | Var id -> ID.to_str id 
  | Num n -> PQNum.num_to_str n 
  | Str s -> "\""^s ^"\""
  | Sym s -> "`" ^ s
  | Unit -> "()"
  | Prim p -> "PRIM(" ^ (Prim.prim_to_str p) ^ ")"
  | Stream (v,t) -> "STREAM<" ^ (value_node_to_str v) ^">"   
  | Lam fundef -> 
    Format.sprintf "fun input:[%s] output:[%s] { \n @[<hov 2> %s @] \n}" 
      (String.concat ", " (List.map ID.to_str fundef.input_ids))
      (String.concat ", " (List.map ID.to_str fundef.output_ids))
      (block_to_str fundef.body)
  
and value_node_list_to_str ?(sep=",") vs = 
  String.concat (sep^" ") (List.map value_node_to_str vs)
  
and value_list_to_str ?(sep=",") vs = 
  String.concat (sep^" ") (List.map value_to_str vs)

let fundef_to_str (fundef:fundef) = 
  sprintf "%s (%s)=>(%s) { \n %s \n }"
    (FnId.to_str fundef.fundef_id) 
    (typed_id_list_to_str fundef.tenv fundef.input_ids)
    (typed_id_list_to_str fundef.tenv fundef.output_ids)
    (block_to_str ~space:"\t" ~tenv:fundef.tenv fundef.body)
  
 
(* if a function contains nothing but a map, extracted the nested 
   function being mapped 
*) 
let extract_nested_map_fn_id (fundef : fundef) = 
  match fundef.body with 
    (* LEGACY-- should delete *) 
    | [{stmt=
          Set(_,{exp=App(map, {value=GlobalFn fnId}::dataArgs)})
       }
      ] when map.value = Prim (Prim.ArrayOp Prim.Map) ->
        Some fnId 
    | [{stmt=Set(_, {exp=Map({closure_fn=fnId}, _)})}] -> Some fnId 
    | _ -> None   
(*  
let rec collect_assigned_ids = function  
  | (Set (ids, _))::rest ->  ids @ (collect_assigned_ids rest)
  | (If (_, tBlock, fBlock, ifGate))::rest -> 
      let tList = collect_assigned_ids tBlock in 
      let fList = collect_assigned_ids fBlock in 
      let gateList = ifGate.if_output_ids in 
      tList @ fList @ gateList @ (collect_assigned_ids rest)
  | (WhileLoop (_, body,loopGate))::rest ->
      let bodyList = collect_assigned_ids body in 
      loopGate.loop_outputs @ bodyList @ (collect_assigned_ids rest) 
  | _::rest -> collect_assigned_ids rest 
*)

let mk_fundef  ?(tenv=ID.Map.empty) ~input_ids ~output_ids ~body =
  let inTypes = 
    List.map (fun id -> ID.Map.find_default id tenv DynType.BottomT) input_ids 
  in 
  let outTypes = 
    List.map (fun id -> ID.Map.find_default id tenv DynType.BottomT) output_ids 
  in 
  { 
    body = body; 
    tenv = tenv; 
    input_ids = input_ids; 
    output_ids = output_ids; 
    fundef_type = DynType.FnT(inTypes, outTypes); 
    fundef_id = FnId.gen()  
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

let mk_if ?src condVal tBlock fBlock gate = 
  { stmt = If(condVal, tBlock, fBlock, gate); 
    stmt_src=src; 
    stmt_id = StmtId.gen() 
  } 

(* get the id of a variable value node *) 
let get_id valNode = match valNode.value with 
  | Var id -> id 
  | other -> failwith $ Printf.sprintf 
     "[SSA->get_id] expected variable, received %s"
     (value_to_str other)

(* get the ids from a list of variable value nodes *) 
let get_ids vars = List.map get_id vars 

let get_fn_id valNode = match valNode.value with 
  | GlobalFn fnId -> fnId
  | other -> failwith $ 
      Printf.sprintf 
        "[SSA->get_fn_id] expected global function, received %s"
        (value_to_str other)
  

let get_fn_ids valNodes = List.map get_fn_id valNodes  



(***
    helpers for values 
 ***)


let mk_val ?src ?(ty=DynType.BottomT) (v:value) : value_node = 
  { value = v; value_src = src; value_type = ty }

let mk_var ?src ?(ty=DynType.BottomT) (id:ID.t) : value_node = 
  { value = Var id; value_src = src; value_type = ty }    

let mk_op ?src ?ty op = mk_val ?src ?ty (Prim op) 

let mk_globalfn ?src ?(ty=DynType.BottomT) (id:FnId.t) : value_node=
  { value = GlobalFn id; value_src = src; value_type = ty } 

let mk_num ?src ?ty n = 
  let ty = match ty with 
    | None -> PQNum.type_of_num n 
    | Some t -> t 
  in 
  mk_val ?src ~ty (Num n)

let mk_bool ?src b = mk_num ?src ~ty:DynType.BoolT (PQNum.Bool b)
let mk_int32 ?src i = mk_num ?src ~ty:DynType.Int32T (PQNum.Int32 (Int32.of_int i))
let mk_float32 ?src f = mk_num ?src ~ty:DynType.Float32T (PQNum.Float32 f)
let mk_stream ?src v t = mk_val ~ty:t (Stream(v,t))  
  
(*** 
    helpers for expressions 
 ***) 

let map_default_types optTypes values = 
  match optTypes with 
    | None -> List.map (fun vNode -> vNode.value_type) values 
    | Some ts -> ts

let mk_app ?src ?types fn args =
  let retTypes = match types, fn.value_type with 
    | Some types, _ -> types 
    | None, DynType.FnT(_, types) -> types 
    | _ -> [DynType.BottomT]
  in 
  { exp=App(fn,args); exp_src = src; exp_types = retTypes }  

let mk_arr ?src ?types elts =
  let argTypes = map_default_types  types elts in
  IFDEF DEBUG THEN 
     assert (List.length argTypes > 0); 
     assert (List.for_all ((=) (List.hd argTypes)) (List.tl argTypes));
  ENDIF;   
  { exp=Arr elts; exp_src=src; exp_types = [DynType.VecT (List.hd argTypes)] } 
 
let mk_val_exp ?src ?ty (v: value) =
  let ty' = match ty with 
    | None -> DynType.BottomT 
    | Some ty -> ty 
  in 
  { exp=Values [mk_val ?src v]; exp_src=src; exp_types = [ty'] } 

let mk_vals_exp ?src ?types ( vs : value list) =
  let valNodes = List.map (mk_val ?src) vs in 
  let types' = map_default_types types valNodes in 
  { exp = Values valNodes; exp_src = src; exp_types=types' } 

let mk_arr_idx ?src ?(types=[DynType.BottomT]) lhs indices =
  { exp = ArrayIndex(lhs, indices); exp_src=src; exp_types=types} 
        
let mk_cast ?src t v = 
  { exp = Cast(t, v); exp_types = [t]; exp_src = src }      

let mk_exp ?src ?types exp =
  (* WARNING--- function calls may need more than 1 return type, in which 
     case the default [BottomT] is wrong 
  *) 
  let types' = match types, exp with 
    | Some ts, _ -> ts
    | None, Values vs -> List.fill DynType.BottomT vs
    | _ -> [DynType.BottomT] 
  in 
  { exp= exp; exp_types = types'; exp_src = src} 

let empty_stmt = mk_set [] (mk_vals_exp [])

let is_empty_stmt stmtNode =
  match stmtNode.stmt with 
    | Set ([], {exp=Values[]})->true
    | _ -> false 
   
let empty_block : block = [||]
let append_block = Array.append 
let block_of_list = Array.of_list 
let block_iter f = Array.iter f 
let block_length = Array.length
let block_idx = Array.get 
 