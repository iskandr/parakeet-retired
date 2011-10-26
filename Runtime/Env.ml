(* stack of environments which map identifiers to interpreter values*)
(* while also counting reference counts to distinct pieces of data  *)
 

type t = {
    envs :  Value.t ID.Map.t Stack.t; 
    data_scopes : DataId.Set.t Stack.t; 
    refcounts : (DataId.t, int) Hashtbl.t;
}

let create () = { 
    envs = Stack.create ();
    data_scopes = Stack.create (); 
    refcounts = Hashtbl.create 1001;  
} 

let state = create ()

(* if an identifier is already bound in the given env, then decrease *)
(* the refcount of its values *) 
let decref_old_binding env id = 
    if ID.Map.mem id env then dec_ref (ID.Map.find id env)

let set_binding id rhs =
    (* be sure to increment ref counts first, to avoid deleting something 
       when it was also the old value of a variable *) 
    inc_ref rhs; 
    let env = Stack.pop state.envs in
    decref_old_binding env id; 
    Stack.push (ID.Map.add id rhs env) memState.envs 

let curr_env () = Stack.top state.envs  

let push_env () = 
    if Stack.is_empty state.envs then Stack.push ID.Map.empty memState.envs 
    else Stack.push (curr_env ()) memState.envs

let pop_env () = ignore (Stack.pop memState.envs) 
   

let push_data_scope () =
    Stack.push DataId.Set.empty memState.data_scopes

let rec data_id_set_from_values = function 
    | [] -> DataId.Set.empty  
    | (Value.Data id)::vs -> DataId.Set.add id (data_id_set_from_values vs)
    | (Value.Scalar _ )::vs -> data_id_set_from_values vs
    | (Value.Array arr)::vs -> 
        ID.Set.union 
          (data_id_set_from_values (Array.to_list arr)) 
          (data_id_set_from_values vs)

let pop_data_scope escaping_values = 
    let dataScope = Stack.pop state.data_scopes in 
    let excludeSet = data_id_set_from_values escaping_values in
    (* remove the escaping IDs from the scope before freeing its contents *)
    let prunedScope = DataId.Set.diff dataScope excludeSet in
    DataId.Set.iter (dec_data_id_ref memState) prunedScope;
    (* we still decrement the counts on the exclude set, we 
       just expect them to be incremented again by an assignment 
    *) 
    DataId.Set.iter 
      (fun id -> set_refcount id (get_refcount id - 1))
      excludeSet
    ; 
    (* dump the excluded ids into the older data scope *)
    if Stack.is_empty state.data_scopes then 
      Stack.push excludeSet state.data_scopes 
    else   
      let olderScope = Stack.pop memState.data_scopes in
      Stack.push (DataId.Set.union excludeSet olderScope) memState.data_scopes 

let enter_scope () = 
    push_data_scope (); 
    push_env ()

let exit_scope escaping_values = 
    pop_data_scope escaping_values; 
    pop_env ()
  
let set_bindings memState ids vals = 
    List.iter inc_ref vals;
    let env = Stack.pop memState.envs in
    List.iter (decref_old_binding env) ids;   
    Stack.push (ID.Map.extend env ids vals) memState.envs  

let lookup memState id =
    let env = curr_env () in 
    if ID.Map.mem id env then ID.Map.find id env
    else failwith $ Printf.sprintf "[Env] variable %s not found!" (ID.to_str id) 
    
  (*
module Scope = struct 



  let dec_old_binding_value memState env id = 
    if ID.Map.mem id env then (   
      dec_ref memState (ID.Map.find id env)
    )
    
  

    

 
end
include Scope
*) 