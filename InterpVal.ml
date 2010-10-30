module DataId = UID.Make(struct let to_str x = "data" ^ (string_of_int x) end)

type t = Data of DataId.t | Closure of ID.t * t list

let rec to_str = function 
  | Data dataId -> DataId.to_str dataId
  | Closure (fnId, args) -> 
    Printf.sprintf "closure{%d, [%s]}"
      fnId (String.concat ", " (List.map to_str args))
