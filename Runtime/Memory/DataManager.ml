
open Base
open Data 

type data_table = (DataId.t, Ptr.t) Hashtbl.t 

let memspace_tables : data_table MemId.Map.t ref = ref MemId.Map.empty

(* if we already have a table for the given id then return it, *)
(* otherwise create a  new one *)   
let get_memspace_table memid : data_table =
    try MemId.Map.find memid !memspace_tables with _ -> begin
        let new_table : data_table = Hashtbl.create 1001 in 
        memspace_tables := MemId.Map.add memid new_table !memspace_tables; 
        new_table 
    end  

let register_ptr (p:Ptr.t) : DataId.t =
    let memId = p.Ptr.memspace in   
    let table = get_memspace_table memId in
    let dataId = DataId.gen() in
    Hashtbl.add table dataId p; 
    dataId 
    
let from_memspace (v : Ptr.t Value.t) = 
    Value.map register_ptr v 
    
let to_memspace memId v =
    let table = get_memspace_table memId in 
    let lookup dataId = 
        match Hashtbl.find_option table dataId with 
        | Some ptr ->  ptr  
        | None -> failwith $ "Could not find data id: " ^ DataId.to_str dataId
    in 
    Value.map lookup v 
  