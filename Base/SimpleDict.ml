open Base 

(* abstract over possible representations of maps in case 
   I want to change from Hashtbl to PMap. This is a funny ad-hoc
   encoding of objects (record + hidden mutable state), but I prefer
   it over the heavy-weight object syntax of OCaml. 
*)
type ('a,'b) dict = {
    add : 'a ->'b->unit; 
    remove : 'a -> unit; 
    find: 'a ->'b; 
    mem:'a->bool;
    enum : unit -> ('a * 'b) Enum.t;
    iter : ('a -> 'b -> unit) -> unit; 
  }

let id x = x 

(* 
   allow for options to use the SimpleDict interface with existing Hashtables.
*) 
let create_dict_wrapper h ~key_transform ~inv_key_transform = 
  {
    find = (fun x -> Hashtbl.find h (key_transform x)); 
    add= (fun x y -> Hashtbl.add h (key_transform x) y); 
    remove = (fun x -> Hashtbl.remove h (key_transform x)); 
    mem= (fun x -> Hashtbl.mem h (key_transform x)); 
    enum = (fun () -> 
               Enum.map 
               (fun (x,y) -> (inv_key_transform x),y)
               (Hashtbl.enum h)
           );
    iter= (fun f -> 
             Hashtbl.iter (fun x y -> f (inv_key_transform x) y) h
          )
  }

let create_dict () = 
  create_dict_wrapper (Hashtbl.create 127) 
    ~key_transform:id ~inv_key_transform:id
  