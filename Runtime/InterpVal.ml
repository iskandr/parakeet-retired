open Base 

module DataId = UID.Make(struct let to_str x = "data" ^ (string_of_int x) end)

type t = 
  | Data of DataId.t
  (*| Closure of ID.t * t list*) 
  | Scalar of PQNum.num
  | Array of t array 

let rec to_str = function 
  | Data dataId -> DataId.to_str dataId
  | Scalar n -> PQNum.num_to_str n
  | Array a -> Printf.sprintf "<array of %d elts>" (Array.length a)
  (*| Closure (fnId, args) -> 
    Printf.sprintf "closure{%d, [%s]}"
      fnId (String.concat ", " (List.map to_str args))
   *)

let to_int = function 
  | Scalar n -> PQNum.to_int n 
  | other -> failwith $ Printf.sprintf  
           "Can't get integer from non-scalar interpreter value: %s"
           (to_str other) 

let to_bool = function 
  | Scalar (PQNum.Bool b) -> b 
  | other -> failwith $ Printf.sprintf
           "Can't get boolean from interpreter value: %s"
           (to_str other) 

let to_num = function 
  | Scalar n -> n 
  | other -> failwith $ Printf.sprintf
           "Can't get scalar from interpreter value: %s"
           (to_str other)

let of_bool b = Scalar (PQNum.Bool b) 
let of_int i = Scalar (PQNum.Int32 (Int32.of_int i))


