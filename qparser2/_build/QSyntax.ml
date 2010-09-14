
type source_info = unit 

type syntax = 
    | StrLit of string 
    | SymLit of string 
    | FloatLit of float
    | RealLit of float
    | IntLit of Int32.t
    | ShortLit of int
    | BitLit of int
    | Id of string 
    | LamExp of string list * syntax_node
    | SimpleLamExp of syntax_node  (* no variables provided *) 
    | ArrExp of syntax_node list 
    | AppExp of  syntax_node * (syntax_node list)
    | ControlExp of string * (syntax_node list) 
    | BlockExp of syntax_node list 
    | DefExp of string * syntax_node 
    | SetIdxExp of string * (syntax_node list) * syntax_node 
    | TimeExp of syntax_node 
      
and syntax_node = syntax * source_info

let mk_source_info x y = () 

let mk_syntax_node ?(line=0) ?(col=0) exp  = exp, mk_source_info line col 

open Printf 

let rec syntax_to_str = function 
  | StrLit str -> "\"" ^ str ^ "\""  
  | SymLit sym -> "`" ^ sym  
  | FloatLit f  
  | RealLit f -> string_of_float f
  | IntLit i -> Int32.to_string i 
  | ShortLit i
  | BitLit i -> string_of_int i 
  | Id name -> "ID:" ^ name 
  | LamExp (args, body) -> "<lam>"
  | SimpleLamExp body -> sprintf "{%s}" (node_to_str body) 
  | ArrExp elts -> "<arr>" 
  | AppExp (lhs, args) ->
       sprintf "App(%s, [%s])" (node_to_str lhs) (nodes_to_str "; " args)
  | ControlExp (name, args) -> "<control>" 
  | BlockExp stmts -> sprintf "[%s]" (nodes_to_str "; " stmts)
  | DefExp (name, rhs) -> "<def>" 
  | SetIdxExp (name, indices, rhs) -> "<setidx>"
  | TimeExp node -> "\\t "^ (node_to_str node) 
and nodes_to_str sep nodes = String.concat sep (List.map node_to_str nodes)    
and node_to_str (syntax, _) = syntax_to_str syntax 