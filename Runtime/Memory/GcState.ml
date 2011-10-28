

let pinned = ref Int64.Set.empty    
let pin (p : Ptr.t) : unit =
    pinned := Int64.Set.add p#addr !pinned
let unpin (p: Ptr.t) : unit =
    pinned := Int64.Set.remove p#addr !pinned 
let in_use : (Ptr.t, unit) Hashtbl.t = Hashtbl.create 1001 
let free : (Ptr.t, unit) Hashtbl.t = Hashtbl.create 1001
let add_free_ptr (p : Ptr.t) : unit = 
  Hashtbl.add free p#size p 
let find_free_ptr size : Ptr.t option = 
  match Hashtbl.find_option free size with
    | None -> None
    | some_ptr -> 
        Hashtbl.remove free size;
        some_ptr  



let collect () = 
    let active_set = 
      Int64.Set.union (DataManager.trace_active_addresses memspace_id) !pinned 
    in
    Hashtbl.iter (fun addr _ ->  
    
let rec force_til_some = function 
  | [] -> None
  | delayed::rest -> 
    (match Lazy.force delayed with None -> force_til_some rest | other -> other)  
