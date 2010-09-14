type source_info = unit
type syntax =
    StrLit of string
  | SymLit of string
  | FloatLit of float
  | RealLit of float
  | IntLit of Int32.t
  | ShortLit of int
  | BitLit of int
  | Id of string
  | LamExp of string list * syntax_node
  | SimpleLamExp of syntax_node
  | ArrExp of syntax_node list
  | AppExp of syntax_node * syntax_node list
  | ControlExp of string * syntax_node list
  | BlockExp of syntax_node list
  | DefExp of string * syntax_node
  | SetIdxExp of string * syntax_node list * syntax_node
  | TimeExp of syntax_node
and syntax_node = syntax * source_info
val mk_source_info : 'a -> 'b -> unit
val mk_syntax_node : ?line:int -> ?col:int -> 'a -> 'a * unit
val syntax_to_str : syntax -> string
val node_to_str : syntax_node -> string
