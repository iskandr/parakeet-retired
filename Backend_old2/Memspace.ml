
(* encoding trick to put pointers of different types 
   into the same container and later distinguish them 
   by the type tags of distinct exception types
*) 

module type BOXED = sig type t exception E of t end

type 'a t = (module BOXED with type t = 'a)

let create (type s) m = 
  let module Local = struct type t = s exception E of s end in 
  (module Local : BOXED with type t = s)
  