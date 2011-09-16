
type id = int

let max_id = ref 0 
let names = ref []

let get_name id = List.assoc id !names

let register name = 
  let id = !max_id in 
  max_id := id + 1; 
  names := (id, name) :: !names;
  id