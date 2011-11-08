open Base 

type t = int

let gen = Base.mk_gen()


let names = Hashtbl.create 127 
let rev_names = Hashtbl.create 127 

let set_name id name =
    if Hashtbl.mem rev_names name then 
        failwith $ "Memspace name already registered " ^ name
    else (    
        Hashtbl.add names id name; 
        Hashtbl.add rev_names name id
    )  
  
let find_name id = match Hashtbl.find_option names id with 
    | None -> failwith $ "Unregister memory space " ^ (string_of_int id)
    | Some name -> name   

let find_id name = match Hashtbl.find_option rev_names name with 
    | None -> failwith $ "No memory space registered with name " ^ name
    | Some id -> id  

type outer_t = t (* stupid OCaml cyclic type definitions *) 
module Map  = Map.Make(struct type t = outer_t let compare = compare end)  

 