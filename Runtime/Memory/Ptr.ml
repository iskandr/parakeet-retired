
type t = { 
    addr : Int64.t; 
    size : int; 
    memspace : MemId.t; 
}

let to_str {addr;size;memspace} =
    let name = MemId.find_name memspace in   
    Printf.sprintf "<ptr addr=%Ld, size=%d, memspace=%s" addr size name
  
 