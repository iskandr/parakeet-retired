
(* unique identifiers for memory spaces associated with their names *) 

type t = int 

let max_id = ref 0 

let id_to_string = Hashtbl.create 127 
let string_to_id = Hashtbl.create 127 

let register name =
  let id = !max_id in 
  max_id := id + 1;
  if Hashtbl.mem string_to_id name then 
    failwith ("Already registered memory space " ^ name)
  else (
    Hashtbl.add string_to_id name id; 
    Hashtbl.add id_to_string id name; 
    id   
  )

let find_name = Hashtbl.find id_to_string 
let find_id = Hashtbl.find string_to_id 

