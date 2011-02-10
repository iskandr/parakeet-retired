open Base 

type 'a t = ('a, unit) Hashtbl.t

let create = Hashtbl.create 
let mem = Hashtbl.mem
let add set x = Hashtbl.add set x () 
let remove = Hashtbl.remove
let enum x = Enum.map fst (Hashtbl.enum x)  
let iter  f  set = Hashtbl.iter (fun k v -> f k) set 
let fold f set init = Hashtbl.fold (fun k _ acc -> f k acc) set init 
