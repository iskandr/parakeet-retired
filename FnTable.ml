
open Base

type t = (ID.t, SSA.fundef) Hashtbl.t

let add ?(id=ID.gen()) f cache = 
  Hashtbl.add  cache id f; 
  id 
  
let find id cache = Hashtbl.find cache id  
let find_option id cache = 
  if Hashtbl.mem cache id  then Some (Hashtbl.find cache id) else None 
  
let mem id cache = Hashtbl.mem cache id 

let from_list (fns : (ID.t * SSA.fundef) list) : t = Hashtbl.from_list fns

let create (n : int) : t = Hashtbl.create n 