include UID.Make(struct let prefix = "_x" end)

(* useful for SSA merges where undefined variable might get used *)
let undefined = (of_int (-1))
