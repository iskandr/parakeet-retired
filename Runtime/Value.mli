
type t = 
  | Data of DataId.t
  | Scalar of ParNum.t
  | Array of t array 

val to_str : t -> string 
val to_int : t -> int 
val to_bool : t -> bool  
val to_num : t -> ParNum.t 

val of_bool : bool -> t  
val of_int : int -> t 
val of_float : float -> t 

