open Ptr 

type t = { 
    mutable used_ptrs : (Ptr.t, unit) Hashtbl.t;
    mutable pinned : Int64.Set.t; 
    free : (int, Ptr.t) Hashtbl.t;    
} 


let create () =  {
    used_ptrs = Hashtbl.create 1001; 
    pinned = Int64.Set.empty; 
    free_ptrs = Hashtbl.create 1001
}

let pin (s : t) (p : Ptr.t) : unit =
    s.pinned <- Int64.Set.add p.addr s.pinned
    
let unpin (p: Ptr.t) : unit =
    s.pinned <- Int64.Set.remove p.addr s.pinned

let add_used_ptr (s:t) (p:Ptr.t) : unit = 
    Hashtbl.add s.used_ptrs p 

(* any pointer which satisfies the predicate 'f' is kept in the used*)
(* pointer hashtbl, otherwise moved to the free pointer hashtbl *) 
let filter_used_ptrs (s:t) (f : Ptr.t -> bool) : unit =
    let used_ptrs' = Hashtbl.create 1001 in 
    let keep_if_pred p () = 
        if f p then Hashtbl.add used_ptrs' p ()
        else Hashtbl.add s.free p.size p
    in 
    (* put each pointer inside s.used_ptrs either into used_ptrs' or s.free *)     
    Hashtbl.iter keep_if_pred s.used_ptrs;
    s.used_ptrs <- used_ptrs' 

let add_free_ptr (s : t) (p : Ptr.t) : unit = 
    Hashtbl.add s.free p.size p
   
let find_free_ptr (s:t) (size:t) : Ptr.t option = 
  match Hashtbl.find_option s.free size with
    | None -> None
    | some_ptr -> 
        Hashtbl.remove s.free size;
        some_ptr  

let iter_free_ptrs (s:t) (f : Ptr.t -> unit) : unit = Hashtbl.iter s.free f
let clear_free_ptrs (s:t) = Hashtbl.clear s.free   
                
let pinned_addr_set {pinned} = pinned 
