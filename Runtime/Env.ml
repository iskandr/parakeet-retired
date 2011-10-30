(* stack of environments which map identifiers to interpreter values*)
(* while also counting reference counts to distinct pieces of data  *)
 

type env = Value.t ID.Map.t
 
type t = { envs : env Stack.t } 

let create () = { envs = Stack.create() }   

let state = create ()

let curr_env () = Stack.top state.envs 

let push_env () = 
    if Stack.is_empty state.envs then Stack.push ID.Map.empty state.envs 
    else Stack.push (curr_env ()) state.envs

let pop_env () = ignore (Stack.pop state.envs) 

let set_binding id rhs =
    let env = Stack.pop state.envs in
    Stack.push (ID.Map.add id rhs env) state.envs 

let set_bindings ids vals = 
    let env = Stack.pop memState.envs in
    Stack.push (ID.Map.extend env ids vals) state.envs  

let lookup memState id =
    let env = curr_env () in 
    if ID.Map.mem id env then ID.Map.find id env
    else failwith $ Printf.sprintf "[Env] variable %s not found!" (ID.to_str id) 

(*
let rec data_id_set_from_values = function 
    | [] -> ArrayId.Set.empty  
    | (Value.Data id)::vs -> ArrayId.Set.add id (data_id_set_from_values vs)
    | (Value.Scalar _ )::vs -> data_id_set_from_values vs
    | (Value.Array arr)::vs -> 
        ID.Set.union 
          (data_id_set_from_values (Array.to_list arr)) 
          (data_id_set_from_values vs)

*)