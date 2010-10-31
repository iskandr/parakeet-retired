(* unique global identifiers for data items *) 
module DataId = 
  UID.Make(struct let to_str x = "data" ^ (string_of_int x) end)

let gen_data_id = Base.mk_gen() 

type t = 
  | Data of DataId.t
  | Closure of ID.t * t list  

let rec to_str = function 
  | Data id -> DataId.to_str id 
  | Closure (fnid, args) -> 
    Printf.sprintf "closure{%d, [%s]}" fnid
      (String.concat ", " (List.map to_str args))
      