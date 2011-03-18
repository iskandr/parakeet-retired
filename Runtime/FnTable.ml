
open Base

type t = {
  fundefs: (FnId.t, SSA.fundef) Hashtbl.t;
  unoptimized_queue : FnId.t Queue.t;
  arities : (FnId.t, int) Hashtbl.t; 
}

let add ?(opt_queue=true) fundef (cache:t) =
  let id = fundef.SSA.fn_id in 
  Hashtbl.add cache.fundefs id fundef;
  let arity = List.length fundef.SSA.input_ids in
  Hashtbl.add cache.arities id arity;  
  if opt_queue then Queue.add id cache.unoptimized_queue 
  
let find id cache = Hashtbl.find cache.fundefs id  

let find_option id cache = 
  if Hashtbl.mem cache.fundefs id  then 
    Some (Hashtbl.find cache.fundefs id) 
  else None 
  
let mem id cache = Hashtbl.mem cache.fundefs id 

let create (n : int) : t = { 
  fundefs = Hashtbl.create (2*n+1); 
  unoptimized_queue = Queue.create ();
  arities = Hashtbl.create (2*n+1); 
} 

let from_list (fns :  SSA.fundef list) : t = 
  let cache = create (2 * (List.length fns) + 1) in 
  List.iter (fun fundef -> add fundef cache) fns;
  cache
  
let have_unoptimized cache = not $ Queue.is_empty cache.unoptimized_queue

let get_unoptimized cache = 
  IFDEF DEBUG THEN assert (have_unoptimized cache); ENDIF;  
  let id = Queue.pop cache.unoptimized_queue in 
  find id cache  

let get_arity fnId cache  = Hashtbl.find cache.arities fnId 
  
let update fundef cache = 
  let id = fundef.SSA.fn_id in 
  IFDEF DEBUG THEN
    assert (Hashtbl.mem cache.fundefs id); 
  ENDIF; 
  Hashtbl.replace cache.fundefs id fundef    

let get_fundef fnTable valNode = match valNode.SSA.value with 
  (*| SSA.Lam fundef -> fundef*) 
  | SSA.GlobalFn fnId -> find fnId fnTable
  | _ -> failwith "Expected  global function id"