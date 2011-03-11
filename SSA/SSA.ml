(* pp: -parser o pa_macro.cmo *)

open Base

type value = 
  | Var of ID.t
  | Num of PQNum.num 
  | Str of string
  | Sym of string
  | Unit
  | Prim of Prim.prim
  | GlobalFn of FnId.t  
and value_node = { 
  value_type : DynType.t;
  value_src : SourceInfo.t option; 
  value : value 
}
and value_nodes = value_node list   

type exp = 
  (* application of arbitrary values used only in untyped code *) 
  | App of  value_node * value_nodes
  (* construction of arrays and values used by both typed and untyped ssa *) 
  | Arr of value_nodes
  | Values of value_nodes
  (* nodes below are only used after type specialization *) 
  | Cast of DynType.t * value_node  
  | Call of FnId.t * value_nodes 
  | PrimApp of Prim.prim * value_nodes  
  | Map of closure * value_nodes
  | Reduce of closure * closure * value_nodes * value_nodes 
  | Scan of closure * closure * value_nodes * value_nodes 
   
and exp_node = { 
  exp: exp; 
  exp_src : SourceInfo.t option;
  (* because a function applicatin might return multiple values,*)
  (* expressions have multiple types *)  
  exp_types : DynType.t list; 
} 
and closure = {   
  closure_fn: FnId.t; 
  closure_args: value_node list; 
  closure_arg_types: DynType.t list; 
  closure_input_types:DynType.t list; 
  closure_output_types: DynType.t list 
} 

type stmt = 
  | Set of ID.t list * exp_node 
  | SetIdx of ID.t * value_nodes * value_node
  | If of value_node * block * block * phi_nodes
  (* testBlock, testVal, body, loop header, loop exit *)  
  | WhileLoop of block * value_node * block * phi_nodes * phi_nodes 
and stmt_node = { 
    stmt: stmt;
    stmt_src: SourceInfo.t option;
    stmt_id : StmtId.t;  
}
and block = stmt_node Block.t
and phi_node = { 
  phi_id : ID.t;
  phi_left:  value_node;
  phi_right: value_node;
  phi_type : DynType.t; 
  phi_src : SourceInfo.t option; 
}  
and phi_nodes = phi_node list 

type fundef = {
  body: block;
  tenv : tenv;
  input_ids:ID.t list;
  output_ids: ID.t list; 
  fn_input_types : DynType.t list;
  fn_output_types : DynType.t list;
  fn_id : FnId.t; 
}
and tenv = DynType.t ID.Map.t 


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

let value_to_str = function 
  | GlobalFn fnId -> FnId.to_str fnId 
  | Var id -> ID.to_str id 
  | Num n -> PQNum.num_to_str n 
  | Str s -> "\""^s ^"\""
  | Sym s -> "`" ^ s
  | Unit -> "()"
  | Prim p -> "PRIM(" ^ (Prim.prim_to_str p) ^ ")"

let value_node_to_str vNode =
  let valStr = value_to_str vNode.value in  
  if vNode.value_type <> DynType.BottomT then 
    sprintf "%s : %s" valStr (DynType.to_str vNode.value_type)
  else valStr

let rec value_nodes_to_str = function 
  | [] -> ""
  | [v] -> value_node_to_str v
  | v::vs -> (value_node_to_str v) ^ "; " ^ (value_nodes_to_str vs)  
and value_node_list_to_str ?(sep=",") vs = 
  String.concat (sep^" ") (List.map value_node_to_str vs)
  
let value_list_to_str ?(sep=",") vs = 
  String.concat (sep^" ") (List.map value_to_str vs)

let closure_to_str cl =
   (FnId.to_str cl.closure_fn) ^ 
   (if List.length cl.closure_args > 0 then 
      " @ " ^ (value_nodes_to_str cl.closure_args)
    else "")

let exp_to_str expNode = 
  match expNode.exp with  
  | Values vs -> value_nodes_to_str vs 
  | App ({value=Prim op}, args) ->    
    sprintf "%s(%s)" (Prim.prim_to_str op) (value_node_list_to_str args)
  | App (fn, args) -> 
    sprintf "%s(%s)" (value_node_to_str fn)  (value_node_list_to_str args)
  | Arr elts -> "[" ^ (value_node_list_to_str ~sep:";" elts) ^ "]" 
  | Cast(t, v) -> 
      sprintf "cast<%s>(%s)" (DynType.to_str t) (value_node_to_str v)

  | Call (fnId, args) -> 
      sprintf "%s(%s)" (FnId.to_str fnId) (value_nodes_to_str args) 
  | PrimApp (p, args) -> 
      sprintf "prim{%s}(%s)" 
        (Prim.prim_to_str p) 
        (value_nodes_to_str args)     
  | Map (closure, args) -> 
      sprintf "map{%s}(%s)" (closure_to_str closure) (value_nodes_to_str args) 
  | Reduce (initClos, reduceClos, initArgs, args) -> 
      sprintf "reduce%s{%s}(%s | %s)"
        (if initClos.closure_fn <> reduceClos.closure_fn then 
          "{" ^ (closure_to_str initClos) ^ "}"
         else ""
        )
        (closure_to_str reduceClos)
        (value_nodes_to_str initArgs)
        (value_nodes_to_str args)
  | Scan (initClos, scanClos, initArgs, args) -> 
      sprintf "scan%s{%s}(%s | %s)"
        (if initClos.closure_fn <> scanClos.closure_fn then 
          "{" ^ (closure_to_str initClos) ^ "}"
         else ""
        )
        (closure_to_str scanClos)
        (value_nodes_to_str initArgs)
        (value_nodes_to_str args)    

let phi_node_to_str ?(space="") phiNode = 
  Printf.sprintf "%s%s <- phi(%s, %s)"
    space 
    (ID.to_str phiNode.phi_id)
    (value_node_to_str phiNode.phi_left)
    (value_node_to_str phiNode.phi_right)

let phi_nodes_to_str ?(space="") phiNodes = 
  String.concat "\n" (List.map (phi_node_to_str ~space) phiNodes)
    
let rec block_to_str ?(space="") ?(tenv=ID.Map.empty) block = 
  Block.to_str (stmt_node_to_str ~space ~tenv) block 
and stmt_node_to_str ?(space="") ?(tenv=ID.Map.empty) stmtNode = 
  let str = match stmtNode.stmt with 
  | Set (ids, rhs) -> 
    sprintf "%s = %s " (typed_id_list_to_str tenv ids) (exp_to_str rhs)
  | SetIdx _ -> "<set-idx>" 
  | If (cond,tBlock,fBlock, phiNodes) ->      
    Printf.sprintf "if %s \nthen %s \nelse %s"
      (value_node_to_str cond)
      (block_to_str ~space:("\t"^space) ~tenv tBlock)
      (block_to_str ~space:("\t"^space) ~tenv fBlock)
      
  | WhileLoop (testBlock, testVal, body, header,exit) ->
      let space' =  "\t"^space in 
      let headerStr =
        Printf.sprintf "%s  <Header>:\n%s"
          space
          (phi_nodes_to_str ~space:space' header)
      in  
      
      let loopTestStr = 
        Printf.sprintf "%s  <TestBlock>:%s\n%s  <TestVal>: %s"
          space
          (block_to_str ~space:space' ~tenv testBlock)
          space
          (value_node_to_str testVal)
      in 
      let exitStr =
        Printf.sprintf "%s  <Exit>:\n%s"
          space 
          (phi_nodes_to_str ~space:space' exit)
      in  
      let bodyStr = 
        Printf.sprintf "%s  <Body>:%s" 
          space 
          (block_to_str ~space:space' ~tenv body)
      in 
          
      Printf.sprintf "while\n%s\n%s\n%s\n%s"
        headerStr
        loopTestStr
        bodyStr 
        exitStr
       
  in space ^ str

let fundef_to_str (fundef:fundef) = 
  sprintf "%s (%s)=>(%s) { \n %s \n }"
    (FnId.to_str fundef.fn_id) 
    (typed_id_list_to_str fundef.tenv fundef.input_ids)
    (typed_id_list_to_str fundef.tenv fundef.output_ids)
    (block_to_str ~space:"\t" ~tenv:fundef.tenv fundef.body)



(* if a function contains nothing but a map, extracted the nested 
   function being mapped 
*) 
let extract_nested_map_fn_id (fundef : fundef) =
  if Block.length fundef.body <> 1 then None 
  else match (Block.idx fundef.body 0).stmt with 
    | Set(_,{exp=App(map, {value=GlobalFn fnId}::_)})
      when map.value = Prim (Prim.Adverb Prim.Map) -> Some fnId
    | Set(_, {exp=Map({closure_fn=fnId}, _)}) ->  Some fnId 
    | _ -> None 
      
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
    fn_input_types = inTypes;
    fn_output_types = outTypes; 
    fn_id = FnId.gen()  
  }  

  


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

let mk_primapp ?src prim outTypes args =
  { exp = PrimApp (prim, args); exp_src = src; exp_types = outTypes}  

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
    | None, Values vs -> List.fill DynType.BottomT vs
    | _ -> [DynType.BottomT] 
  in 
  { exp= exp; exp_types = types'; exp_src = src} 

let mk_call ?src fnId outTypes args  = 
  { exp = Call(fnId, args); exp_types = outTypes; exp_src=src}

let mk_map ?src closure args = 
  { exp = Map(closure, args); 
    exp_types = List.map (fun t -> DynType.VecT t) closure.closure_output_types; 
    exp_src = src
  } 
let mk_reduce ?src initClosure reduceClosure initArgs args = 
  { 
    exp = Reduce(initClosure, reduceClosure, initArgs, args); 
    exp_types = reduceClosure.closure_output_types; 
    exp_src = src; 
  } 
  
let mk_scan ?src initClosure scanClosure initArgs args = 
  { 
    exp = Scan(initClosure, scanClosure, initArgs, args); 
    exp_types = scanClosure.closure_output_types; 
    exp_src = src; 
  }
   
let mk_closure fundef args = {
  closure_fn = fundef.fn_id; 
  closure_args = args; 
  closure_arg_types = List.map (fun v -> v.value_type) args; 
  closure_input_types = List.drop (List.length args) fundef.fn_input_types; 
  closure_output_types = fundef.fn_output_types; 
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

(***
   helpers for phi-nodes 
  
 ***)


let mk_phi ?src ?ty id left right = 
  { 
    phi_id = id; 
    phi_left = left; 
    phi_right = right;
    phi_type = Option.default DynType.BottomT ty; 
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
      

(* assume a block contains only phi, collect the IDs and 
   either the left or right values 
*) 
let rec collect_phi_values chooseLeft = function 
  | [] -> [], [] 
  | p::ps -> 
    let ids, valNodes = collect_phi_values chooseLeft ps in 
    let currVal = if chooseLeft then p.phi_left else p.phi_right in 
    p.phi_id :: ids, currVal :: valNodes 
    

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
let empty_stmt = mk_set [] (mk_vals_exp [])

let is_empty_stmt stmtNode =
  match stmtNode.stmt with 
    | Set ([], {exp=Values[]})->true
    | _ -> false 

  

