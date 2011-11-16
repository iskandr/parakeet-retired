
open Base
open Ptr 


let fresh_memspace_id : (unit -> int) = mk_gen ()

let hash_size = 127 

let memspace_fns : (MemId.t, Ptr.raw_fns) Hashtbl.t = Hashtbl.create hash_size 

let get_fns id = match Hashtbl.find_option memspace_fns id with 
  | None -> failwith $ "Unregister memory space " ^ (MemId.id_to_str id)
  | Some fns -> fns 

let gc_states : (MemId.t, GcState.t) Hashtbl.t = Hashtbl.create hash_size 

let get_gc_state memspace_id = Hashtbl.find gc_states memspace_id 

let registered = ref [] 

let all_memspace_ids () = !registered

let register name fns = 
    let id = MemId.register name in
    registered := id ::!registered;  
    Hashtbl.add memspace_fns id fns;
    Hashtbl.add gc_states id (GcState.create());
    id   

let trace_free_ptrs memspace_id  =
    let gc_state = get_gc_state memspace_id in
    let pinned : Int64.Set.t = GcState.pinned_addr_set gc_state in
    let active_set = Int64.Set.union  (Env.active_addr_set memspace_id) pinned in
    GcState.filter_used_ptrs gc_state (fun p -> Int64.Set.mem p.addr active_set)  
    
let delete_free_ptrs memspace_id  =
  let gc_state = get_gc_state memspace_id in
  GcState.iter_free_ptrs gc_state (fun p -> p.fns.delete p.addr);
  GcState.clear_free_ptrs gc_state 



(* don't try to reuse space, really allocate new pointer *)  
let direct_alloc memspace_id nbytes : Ptr.t option = 
  let fns = get_fns memspace_id in 
  let addr = fns.alloc nbytes in 
  if addr = 0L  then None 
  else Some {addr = addr; size = nbytes; memspace = memspace_id; fns = fns }  

let rec force_til_some = function 
	| [] -> None 
	| x::xs -> 
			let result = Lazy.force x in 
			if result = None then force_til_some xs 
			else result 

let alloc (memspace_id:MemId.t) (nbytes:int) : Ptr.t = 
  let gc_state = get_gc_state memspace_id in 
  let strategy = [
    lazy (GcState.find_free_ptr gc_state nbytes);
    lazy (trace_free_ptrs memspace_id; GcState.find_free_ptr gc_state nbytes);
    lazy (direct_alloc memspace_id nbytes); 
    lazy (delete_free_ptrs memspace_id; direct_alloc memspace_id nbytes)
  ]
  in match force_til_some strategy with 
    | None -> failwith "Allocation failed"
    | Some ptr -> ptr 

let unsafe_delete {memspace; addr} = (get_fns memspace).delete addr 

let pin ptr = GcState.pin (get_gc_state ptr.memspace) ptr 
   
let unpin ptr = GcState.unpin (get_gc_state ptr.memspace) ptr