open Base 


type 'a t = 
  | Array of 'a
  | Scalar of ParNum.t
  | Nested of ('a t) array 
  | Explode of ParNum.t * Shape.t           (* scalar, shape *) 
  | Rotate of 'a t int * int                (* array, dim, offset *) 
  | Shift of 'a t  int * int * ParNum.t     (* array, dim, offset, default *) 
  | Slice of 'a t * int * int * int         (* array, dim, start, end *) 
  | Range of int * int                      (* start, stop *) 

(* since array data is polymorphic it's by default printed as the *)
(* totally uninformative string '<array>'. If you want something more*)
(* specific than that, you'll have to supply your own array printing *)
(* function. *) 
let rec to_str ?(array_to_str=(fun _ -> "<array>")) = function 
  | Scalar n -> ParNum.num_to_str n
  | Array a -> array_to_str a 
  | Nested elts -> 
    Printf.sprintf "[%s]" 
        (String.concat ", " (List.map to_str (Array.to_list a))) 
  | Explode (n, s) -> 
        Printf.printf "explode(%s, %s)" (ParNum.to_str n) (Shape.to_str s)  
  | Rotate (a, dim, offset) ->
        Printf.printf "rotate(%s, dim=%d, offset=%d)" 
            (to_str array_to_str a) dim offset    
  | Shift (a, dim, offset, default) -> 
        Printf.printf "rotate(%s, dim=%d, offset=%d, default=%s)" 
            (to_str array_to_str a) dim offset (ParNum.to_str default)

  | Slice (a, dim, start, stop) -> 
        Printf.printf "slice(%s, dim=%d, start=%d, stop=%d)"
            (to_star array_to_str a) 
  | Range (start, stop) -> 
        Printf.printf "range(from=%d, to=%d)" start stop 

let rec map f = function
  | Array a -> Array (f a) 
  | Nested elts -> Nested (Array.map (map f) elts) 
  | Rotate (a, dim, offset) -> Rotate (map f a, dim offset)
  | Shift (a, dim, offset, default) -> Shift (map f a, dim, offset, default) 
  | Slice (a, dim, start, stop) -> Slice (map f a, dim, start, offset)
  | other -> other  
  
    
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

