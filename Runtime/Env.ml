open Base

(* stack of environments which map identifiers to interpreter values*)
(* while also counting reference counts to distinct pieces of data  *)


type env = (DataId.t Value.t) ID.Map.t
let envs : env Stack.t = Stack.create ()

let curr_env () = Stack.top envs

let push_env () =
    if Stack.is_empty envs then Stack.push ID.Map.empty envs
    else Stack.push (curr_env ()) envs

let pop_env () = ignore (Stack.pop envs)

let lookup id =
    let env = curr_env () in
    if ID.Map.mem id env then ID.Map.find id env
    else failwith $ Printf.sprintf "[Env] variable %s not found!" (ID.to_str id)


let set_binding id rhs =
    let env = Stack.pop envs in
    Stack.push (ID.Map.add id rhs env) envs

let set_bindings ids vals =
    let env = Stack.pop envs in
    Stack.push (ID.Map.extend env ids vals) envs

let active_values () =
		let env = curr_env () in
		ID.Map.values env

let active_data_ids () = Value.collect_list (active_values () )

let active_ptrs memId =
		let dataIds = active_data_ids() in
		let aux ptrs dataId =
				match DataManager.id_to_ptr_option dataId memId with
					| None -> ptrs
					| Some ptr -> ptr::ptrs
	  in
		List.fold_left aux [] dataIds

let active_addrs memId = List.map Ptr.addr (active_ptrs memId)
let active_addr_set memId = Int64.Set.of_list (active_addrs memId)