
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
  
let get_name id name = Hashtbl.find names id  

 