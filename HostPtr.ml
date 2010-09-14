
(* 
  For now assume we're always working on a 64-bit system. Will have
  to eventually do something more flexible and allow for 32-bit pointers. 
  Include the Base.Int64 module instead of the standard Int64 to use
  the functions I've added. 
*)

include Base.Int64

let of_int64 x = x

