open Base
open Printf
open Prim 

type ast_info = { 
    mutable defs_local : string PSet.t;
    mutable defs_global : string PSet.t;

    mutable reads_local : strng PSet.t;
    mutable reads_global : string PSet.t; 
            
    mutable writes_local : string PSet.t;
    mutable writes_global : string PSet.t; 
        
    (* write to disk, print to screen, read from internet, etc... *)
    mutable io : bool;
    
    mutable nested_functions : bool;
    mutable is_function : bool;
}

let mk_ast_info () = {
    defs_local = PSet.empty; 
    defs_global = PSet.empty; 
    reads_local = PSet.empty; 
    reads_global = PSet.empty; 
    writes_local = PSet.empty; 
    writes_global = PSet.empty;          
    io = false; 
    nested_functions = false;
    is_function = false;
}

let combine_ast_info info1 info2 = {    
    defs_local = PSet.union info1.defs_local info2.defs_local;
    defs_global = PSet.union info1.defs_global info2.defs_global;
  
    reads_local = PSet.union info1.reads_local info2.reads_local;
    reads_global = PSet.union info1.reads_global info2.reads_global; 
        
    writes_local = PSet.union info1.writes_local info2.writes_local; 
    writes_global = PSet.union info1.writes_global info2.writes_global; 
        
    io = info1.io || info2.io; 
    nested_functions = info1.nested_functions || info2.nested_functions;
    is_function = info1.is_function || info2.is_function
}

let str_set_to_str set =
    "(" ^ (String.concat ", " (PSet.elements set)) ^ ")" 
    
let info_to_str info = 
    sprintf "{ reads_global: %s; writes_global: %s; io: %s; is_fn: %s; writes_local: %s }"
       (str_set_to_str info.reads_global)
       (str_set_to_str info.writes_global)
       (Bool.to_string info.io) 
       (Bool.to_string info.is_function)
       (str_set_to_str info.writes_local)
       

(* core lambda language + random junk for side effects *)  
type exp = 
    | Lam of (string list) * node
    | Var of string
    | Prim of prim
    | Num of ParNum.t
    | Str of string
    | Sym of string
				
    | App of node * node list 
    | Arr of node list 
    | If of node * node * node
    | Def of string * node
    | SetIdx of string * (node list) * node 
				
    | Block of node list
    | WhileLoop of node * node
    | CountLoop of node * node 
    | Void
      
and node = {
    data:exp;
    src:SrcInfo.t;
    mutable ast_info : ast_info;
}

(* FIX: use a better AST_Info without all this local/global nonsense *) 
let defs node = 
    PSet.union node.ast_info.defs_local node.ast_info.defs_global
let uses node = 
    PSet.union node.ast_info.reads_local node.ast_info.reads_global 



let rec to_str ast = match ast.data with 
  | Lam (ids, body) -> 
        sprintf "fn(%s) {%s}" (String.concat "; " ids) (to_str body)
  | Var name -> name
  | Prim p -> prim_to_str p
  | Sym name -> "Symbol("^ name ^")" 
  | Num (Char c ) -> "'" ^ (Char.to_string c) ^ "'"
  | Num n -> num_to_str n 
  | Str str -> "\"" ^ (String.escaped str)  ^ "\""
  | App (fn, args) -> 
        sprintf "%s(%s)" (to_str fn) (args_to_str ~delim:", " args)
  | Arr elts ->   "[" ^ (args_to_str ~delim:", " elts) ^ "]"
  | Def (name, rhs) -> name ^ " := " ^ (to_str rhs) 
  | SetIdx (name, indices, rhs) -> 
        let argsStr = args_to_str ~delim:", " indices in 
        sprintf "%s[%s] := %s" name argsStr (to_str rhs) 
  | Block nodes -> sprintf "{ %s }" (args_to_str nodes)

  | If(test, tNode, fNode) ->
        sprintf "if %s then %s else %s" 
        (to_str test) (to_str tNode) (to_str fNode)
         
  | Void -> "void"
  | WhileLoop (test,body) -> 
      sprintf "while %s do %s" (to_str test)  (to_str body)
  | CountLoop (count, body) -> 
      sprintf "repeat %s do %s" (to_str count) (to_str body)
      
and args_to_str ?(delim="; ") args = String.concat delim (List.map to_str  args)
 
(*
let rec node_to_q_str ?(inBlock=false) node = 
  match node.data with
  | Lam (ids, body) -> 
     sprintf "{[%s] %s}" 
       (String.concat "; " ids) (node_to_str ~inBlock:false body)
  | Var name -> name
  | Prim p -> prim_to_str p
  | Sym name -> "`"^name 
  | Num (Char c ) -> "\"" ^ (Char.to_string c) ^ "\""
  | Num (Float32 f) -> Float.to_string f ^ "e"
  | Num n -> num_to_str n 
  | Str str -> "\"" ^ (String.escaped str)  ^ "\""
  | App ({data=Prim (Adverb p)}, [fnArg; arg1; arg2])
  | App ({data=App({data=Prim (Adverb p)}, [])}, [fnArg; arg1; arg2])
  | App ({data=App({data=Prim (Adverb p)}, [fnArg])}, [arg1; arg2]) ->
     sprintf "%s %s%s %s"
        (node_to_str arg1)
        (node_to_str fnArg)
        (Prim.adverb_to_str p)
        (node_to_str arg2)
      
  | App(fn,args) -> 
        (node_to_str ~inBlock:true fn)^ 
        "[" ^ (args_to_str  args)  ^"]"
  | Arr [] -> "()"
  | Arr [elt] -> "(enlist " ^ (node_to_str ~inBlock:true elt) ^ ")"
  | Arr elts ->   "(" ^ args_to_str elts ^ ")"
  | Def (name, rhs) -> name ^": "^(node_to_str ~inBlock:true rhs) 
  | SetIdx (name, indices, rhs) -> 
	  name ^ "[" ^ (args_to_str indices) ^ "]: "^ (node_to_str ~inBlock rhs) 
  | Block nodes ->
    let left = if inBlock then "[" else "" in 
    let right = if inBlock then "]" else "" in    
	  left ^ (args_to_str ~delim:";\n  "  nodes) ^  right 
     
  | If(test, tNode, fNode) ->
      let testStr = node_to_str test  in 
      if is_void_recursive fNode then  
        (* special case for if statements which only do something
           on the true branch 
         *)
        sprintf "if[%s;%s]" testStr (node_to_str ~inBlock:false tNode)
      else
        let tStr = node_to_str ~inBlock:true tNode in
        let fStr = node_to_str ~inBlock:true fNode in 
        sprintf "$[%s;%s;%s]" testStr tStr fStr 
  | Void -> "::"
  | WhileLoop (test,body) -> 
      sprintf "while[%s; %s]" 
        (node_to_str ~inBlock:true test) 
        (node_to_str ~inBlock:false body)
  | CountLoop (count, body) -> 
      sprintf "do[%s; %s]"
        (node_to_str ~inBlock:true count)
        (node_to_str ~inBlock:true body)
and q_args_to_str ?(delim="; ") args = 
    String.concat delim (List.map (node_to_str ~inBlock:true) args)
*)