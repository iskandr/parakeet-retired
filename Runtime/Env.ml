(* stack of environments which map identifiers to interpreter values*)
(* while also counting reference counts to distinct pieces of data  *)
 

type env = (DataId.t Value.t) ID.Map.t
let envs : env Stack.t = Stack.create ()   

let curr_env () = Stack.top envs 

let push_env () = 
    if Stack.is_empty envs then Stack.push ID.Map.empty envs 
    else Stack.push (curr_env ()) envs

let pop_env () = ignore (Stack.pop envs) 

let set_binding id rhs =
    let env = Stack.pop envs in
    Stack.push (ID.Map.add id rhs env) envs 

let set_bindings ids vals = 
    let env = Stack.pop envs in
    Stack.push (ID.Map.extend env ids vals) envs  

let lookup memState id =
    let env = curr_env () in 
    if ID.Map.mem id env then ID.Map.find id env
    else failwith $ Printf.sprintf "[Env] variable %s not found!" (ID.to_str id) 




(* map values containing abstract data id's to values containing*)
(* memory-space specific arrays *) 
type data_table = (DataId.t Value.t, Data.t) Hashtbl.t 
let memspace_tables : data_table MemId.Map.t ref = ref (data_table MemId.Map.t)

(* if we already have a table for the given id then return it, *)
(* otherwise create a  new one *)   
let get_memspace_table memid =
    try MemId.Map.mem memid !memspace_tables with _ -> begin
        let new_table : data_table = Hashtbl.create 1001 in 
        memspace_tables := MemId.Map.add memid new_table !memspace_tables; 
        new_table 
    end  

let register data = 
    let table = get_memspace_table data.memspace_id in
    let id = DataId.gen() in 
    let v = Value.Array id in 
    Hashtbl.add table v data;
    v    

let rec from_memspace v = match v with  
  | Value.Array data -> register data
  | Value.Rotate (x, dim, amt) -> Value.Rotate(from_memspace x, dim, amt)
  | Value.Shift(x, dim, amt, default) -> 
        Value.Shift(from_memspace x, dim, amt, default)
  | Value.Slice(x, dim, start, stop) -> 
        Value.Slice(from_memspace x, dim, start, stop)
  | Value.Range _  
  | Value.Scalar _  
  | Value.Explode _ -> v  

let to_memspace memId v =
  let table = get_memspace_table memId in 
  let rec aux v = match v with
    | Value.Range _  
    | Value.Scalar _  
    | Value.Explode _ -> v
    | _ -> assert false
  in aux v 