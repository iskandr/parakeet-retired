
type t = { 
    all_ptrs : (Ptr.t, unit) Hashtbl.t;
    mutable pinned : Int64.Set.t; 
    free : (int, Ptr.t) Hashtbl.t;    
} 


let create () =  {
    all_ptrs = Hashtbl.create 1001; 
    pinned = Int64.Set.empty; 
    free = Hashtbl.create 1001
}

let pin (s : t) (p : Ptr.t) : unit =
    s.pinned <- Int64.Set.add p.addr s.pinned
    
let unpin (p: Ptr.t) : unit =
    s.pinned <- Int64.Set.remove p.addr s.pinned

let add_ptr (s:t) (p:Ptr.t) : unit = 
  Hashtbl.add s.all_ptrs p 

let add_free_ptr (s : t) (p : Ptr.t) : unit = 
  Hashtbl.add s.free p.size p
   
let find_free_ptr (s:t) (size:t) : Ptr.t option = 
  match Hashtbl.find_option s.free size with
    | None -> None
    | some_ptr -> 
        Hashtbl.remove s.free size;
        some_ptr  
