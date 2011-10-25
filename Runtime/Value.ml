open Base 


type t = 
  | Data of DataId.t
  | Scalar of ParNum.t
  | Array of t array 

let rec to_str = function 
  | Data dataId -> DataId.to_str dataId
  | Scalar n -> ParNum.num_to_str n
  | Array a -> Printf.sprintf "[%s]" 
    (String.concat ", " (List.map to_str (Array.to_list a)))
  
let to_int = function 
  | Scalar n -> ParNum.to_int n 
  | other -> failwith $ Printf.sprintf  
           "Can't get integer from non-scalar interpreter value: %s"
           (to_str other) 

let to_bool = function 
  | Scalar (ParNum.Bool b) -> b 
  | other -> failwith $ Printf.sprintf
           "Can't get boolean from interpreter value: %s"
           (to_str other) 

let to_num = function 
  | Scalar n -> n 
  | other -> failwith $ Printf.sprintf
           "Can't get scalar from interpreter value: %s"
           (to_str other)

let of_bool b = Scalar (ParNum.Bool b) 
let of_int i = Scalar (ParNum.Int32 (Int32.of_int i))
let of_float f = Scalar (ParNum.Float32 f)
