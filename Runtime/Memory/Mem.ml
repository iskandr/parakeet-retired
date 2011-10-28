
open Base

type memspace_id = int 

type memspace_functions = { 
    alloc : (int -> Int64.t); 
    free : (Int64.t -> unit); 
    idx_bool : (Int64.t -> int -> bool); 
    idx_char : (Int64.t -> int -> char); 
    idx_int32 : (Int64.t -> int -> Int32.t);  
    idx_int64 : (Int64.t -> int -> Int64.t);  
    idx_float32 : (Int64.t -> int -> float); 
    idx_float64 : (Int64.t -> int -> float); 
} 


let fresh_memspace_id : (unit -> int) = mk_gen()

let hash_size = 17 
let memspace_names : (int, string) Hashtbl.t = Hashtbl.create hash_size

let memspace_name id = match Hashtbl.find_option memspace_names id with 
  | None -> failwith $ "Unregister memory space " ^ (string_of_int id)
  | Some name -> name 


let memspace_fns : (int, memspace_functions) Hashtbl.t = Hashtb.create hash_size 

let get_fns id = match Hashtbl.find_option fns id with 
  | None -> failwith $ "Unregister memory space " ^ (string_of_int id)
  | Some fns -> fns 

let gc_states : (int, GcState.t) Hashtbl.t = Hashtbl.create hash_size 

let register name fns = 
  let id = fresh_memspace_id () in 
  Hashtbl.add memspace_names id name; 
  Hashtbl.add memspace_fns id fns; 
  id   


let alloc nbytes : Ptr.t = 
  let strategy = [
    lazy (find_free_ptr nbytes);
    lazy (collect(); find_free nbytes);
    lazy (raw_alloc nbytes); 
    lazy (delete_all_free(); alloc nbytes)
  ]
  in match force_til_some strategy with 
    | None -> failwith "Allocation failed"
    | Some ptr -> ptr 
  
    

type ptr = { 
    addr : Int64.t; 
    size : int; 
    memspace : memspace_id
}


val alloc : int -> ptr
val unsafe_free : ptr -> unit  

val idx_bool : ptr -> int -> bool 
val idx_char : ptr -> int -> char  
val idx_int32 : ptr -> int -> Int32.t   
val idx_int64 : ptr -> int -> Int64.t   
val idx_float32 : ptr -> int -> float  
val idx_float64 : ptr -> int -> float 

val trace_free_ptrs : memspace_id -> unit 
val delete_free_ptrs : memspace_id -> unit

val pin : ptr -> unit
val unpin : ptr -> unit 