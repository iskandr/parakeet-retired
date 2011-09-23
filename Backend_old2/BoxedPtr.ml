type 'a ptr_ops = { 
    free : 'a -> unit; 
    get_int8 : 'a -> int -> int;  
    get_int16 : 'a -> int -> int; 
    get_int32 : 'a -> int -> Int32.t; 
    get_int64 : 'a -> int -> Int64.t; 
    get_float32 : 'a -> int -> float; 
    get_float64 : 'a -> int -> float;
}
      
module type BOXED_MEMSPACE = sig 
  type t
  val ptr_ops : t ptr_ops 
  exception E of t * t ptr_ops     
end

type 'a memspace = (module BOXED_MEMSPACE with type t = 'a)

let create (type s) m = 
  let module Local = struct type t = s exception E of s end in 
  (module Local : BOXED with type t = s)
  
  
let box (type s) memspace raw = 
  let module M = (val memspace : Memspace.BOXED with type t = s) in
  M.E x  