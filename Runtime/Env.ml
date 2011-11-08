(* stack of environments which map identifiers to interpreter values*)
(* while also counting reference counts to distinct pieces of data  *)
 

type env = (DataId.t Value.t) ID.Map.t
let envs : env Stack.t = Stack.create ()   

let curr_env () = Stack.top envs 

let push_env () = 
    if Stack.is_empty envs then Stack.push ID.Map.empty envs 
    else Stack.push (curr_env ()) envs

let pop_env () = ignore (Stack.pop envs) 

let lookup memState id =
    let env = curr_env () in 
    if ID.Map.mem id env then ID.Map.find id env
    else failwith $ Printf.sprintf "[Env] variable %s not found!" (ID.to_str id) 


let set_binding id rhs =
    let env = Stack.pop envs in
    Stack.push (ID.Map.add id rhs env) envs 

let set_bindings ids vals = 
    let env = Stack.pop envs in
    Stack.push (ID.Map.extend env ids vals) envs  
