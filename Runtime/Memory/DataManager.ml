
open Base
open Data 

(* map values containing abstract data id's to values containing*)
(* memory-space specific arrays *) 
type data_table = (DataId.t Value.t, Data.t) Hashtbl.t 

let memspace_tables : data_table MemId.Map.t ref = ref MemId.Map.empty

(* if we already have a table for the given id then return it, *)
(* otherwise create a  new one *)   
let get_memspace_table memid : data_table =
    try MemId.Map.find memid !memspace_tables with _ -> begin
        let new_table : data_table = Hashtbl.create 1001 in 
        memspace_tables := MemId.Map.add memid new_table !memspace_tables; 
        new_table 
    end  

let register (data : Data.t) = 
    let table = get_memspace_table data.memspace_id in
    let id = DataId.gen() in 
    let v = Value.Array (id,  data.elt_type,  data.shape) in 
    Hashtbl.add table v data;
    v    

let rec from_memspace v = match v with  
  | Value.Array (data, _, _) -> register data
  | Value.Rotate (x, dim, amt) -> Value.Rotate(from_memspace x, dim, amt)
  | Value.Shift(x, dim, amt, default) -> 
        Value.Shift(from_memspace x, dim, amt, default)
  | Value.Slice(x, dim, start, stop) -> 
        Value.Slice(from_memspace x, dim, start, stop)
  | Value.Range (start, stop) -> Value.Range(start, stop)
  | Value.Scalar n -> Value.Scalar n 
  | Value.Explode (n, shape) -> Value.Explode(n, shape)  

let to_memspace memId v =
  let table = get_memspace_table memId in 
  let rec aux v = match v with
    | Value.Range (start, stop) -> Value.Range(start, stop)  
    | Value.Scalar n -> Value.Scalar n 
    | Value.Explode (n, shape) -> Value.Explode (n, shape)
    | Value.Array (id, elt_t, shape) -> 
        (match Hashtbl.find_option table v with 
          | Some data -> Value.Array(data, elt_t, shape)
          | None -> failwith $ "Could not find " ^ (Value.to_str v)
        ) 
    | _ -> assert false
  in aux v 
  
let dissociate _ = assert false 