
open Base

type memspace_fns = { 
    alloc : (int -> Int64.t); 
    free : (Int64.t -> unit); 
    idx_bool : (Int64.t -> int -> bool); 
    idx_char : (Int64.t -> int -> char); 
    idx_int32 : (Int64.t -> int -> Int32.t);  
    idx_int64 : (Int64.t -> int -> Int64.t);  
    idx_float32 : (Int64.t -> int -> float); 
    idx_float64 : (Int64.t -> int -> float); 
}

let fresh_memspace_id : (unit -> int) = mk_gen ()

let hash_size = 17 

let memspace_names : (MemId.t, string) Hashtbl.t = Hashtbl.create hash_size

let memspace_name id = match Hashtbl.find_option memspace_names id with 
  | None -> failwith $ "Unregister memory space " ^ (string_of_int id)
  | Some name -> name 


let memspace_fns : (MemId.t, memspace_fns) Hashtbl.t = Hashtb.create hash_size 

let get_fns id = match Hashtbl.find_option fns id with 
  | None -> failwith $ "Unregister memory space " ^ (string_of_int id)
  | Some fns -> fns 


let gc_states : (MemId.t, GcState.t) Hashtbl.t = Hashtbl.create hash_size 

let get_gc_state memspace_id = Hashtbl.find gc_states memspace_id 

let register name fns = 
  let id = fresh_memspace_id () in 
  Hashtbl.add memspace_names id name; 
  Hashtbl.add memspace_fns id fns;
  Hashtbl.add gc_states id (GcState.create());
  id   

(* don't try to reuse space, really allocate new pointer *)  
let direct_alloc memspace_id nbytes : Ptr.t option = 
  let fns = get_fns memspace_id in 
  let addr = fns.alloc nbytes in 
  if addr = 0L  then None 
  else Some {Ptr.addr = addr; size = nbytes; memspace = memspace_id }  

let alloc memspace_id nbytes : Ptr.t = 
  let gc_state = get_gc_state memspace_id in 
  let strategy = [
    lazy (GcState.find_free_ptr gc_state nbytes);
    lazy (collect(); GcState.find_free_ptr gc_state nbytes);
    lazy (direct_alloc memspace_id nbytes); 
    lazy (delete_all_free(); direct_alloc memspace_id nbytes)
  ]
  in match force_til_some strategy with 
    | None -> failwith "Allocation failed"
    | Some ptr -> ptr 


let idx_bool ptr idx =  false
let idx_char ptr idx =   'b'
let idx_int32 ptr idx =     Int32.zero 
let idx_int64 ptr idx =     Int64.zero 
let idx_float32 ptr idx =   0.0
let idx_float64 ptr idx =   0.0 

let trace_free_ptrs memspace_id  = ()  
let delete_free_ptrs : memspace_id = () 

let pin ptr = 
  GcState.pin (get_gc_state ptr.memspace) ptr 
   
let unpin ptr = 
  GcState.unpin (get_gc_state ptr.memspace) ptr