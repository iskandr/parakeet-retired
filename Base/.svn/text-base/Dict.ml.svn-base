open Base 

(* a dictionary which wraps around a hashtbl *)  
class ['a, 'b, 'c] dict_wrapper 
        (hash : ('a,'b) Hashtbl.t)  
        (key_transform : 'c -> 'a)
        (inv_key_transform : 'a -> 'c) = object 
  method find (x:'c) =  Hashtbl.find hash (key_transform x) 
  method remove x =Hashtbl.remove hash (key_transform x) 
  method mem x = Hashtbl.mem hash (key_transform x) 
  method enum = Enum.map (fun (x,y) -> 
                           (inv_key_transform x),y) (Hashtbl.enum hash)
  method iter (f:'c -> 'b -> unit) =  Hashtbl.iter (fun x y -> f (inv_key_transform x) y) hash
  
  method add (x:'c) (y:'b) = ignore y; (Hashtbl.add hash (key_transform x) y) 
end  


let id x = x 

(* 
   allow for options to use the SimpleDict interface with existing Hashtables.
*) 
class ['a,'b] dict = object 
  inherit ['a,'b,'a] dict_wrapper (Hashtbl.create 127) id id
end
