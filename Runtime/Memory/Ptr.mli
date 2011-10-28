(* pulled out of Mem.ml to break recursion between Mem and DataManager *) 

type t = { 
    addr : Int64.t; 
    size : int; 
    memspace : MemId.t; 
}

val to_str : t -> string 