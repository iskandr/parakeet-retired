include UID.Make(struct let to_str id = "_x" ^ (string_of_int id) end)


(* useful for SSA merges where undefined variable might get used *) 
let undefined = (of_int (-1))  
