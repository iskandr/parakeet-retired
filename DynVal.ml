(* unique global identifiers for data items *) 
module DataId = UID.Make(struct let to_str x = "_data" ^ (string_of_int x) end)

type dyn_val = 
  | Data of DataId.t
  | Closure of ID.t * dyn_val list  
