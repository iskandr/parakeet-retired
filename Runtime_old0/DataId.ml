open Base
 
include UID.Make(struct let to_str x = "data" ^ (string_of_int x) end)