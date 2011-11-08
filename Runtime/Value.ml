open Base 


type 'a t = 
  | Array of 'a  * Type.elt_t * Shape.t
  | Scalar of ParNum.t
  | Nested of ('a t) array 
  | Explode of ParNum.t * Shape.t           (* scalar, shape *) 
  | Rotate of 'a t * int * int                (* array, dim, offset *) 
  | Shift of 'a t *  int * int * ParNum.t     (* array, dim, offset, default *) 
  | Slice of 'a t * int * int * int         (* array, dim, start, end *) 
  | Range of int * int                      (* start, stop *) 

(* since array data is polymorphic it's by default printed as the *)
(* totally uninformative string '<array>'. If you want something more*)
(* specific than that, you'll have to supply your own array printing *)
(* function. *) 
let rec to_str ?(array_to_str=(fun _ -> "<array>")) = function 
  | Scalar n -> ParNum.to_str n
  | Array (a, _, _) -> array_to_str a 
  | Nested elts -> 
    Printf.sprintf "[%s]" 
        (String.concat ", " (List.map to_str (Array.to_list elts))) 
  | Explode (n, s) -> 
        Printf.sprintf "explode(%s, %s)" (ParNum.to_str n) (Shape.to_str s)  
  | Rotate (a, dim, offset) ->
        Printf.sprintf "rotate(%s, dim=%d, offset=%d)" 
            (to_str ~array_to_str a) dim offset    
  | Shift (a, dim, offset, default) -> 
        Printf.sprintf "rotate(%s, dim=%d, offset=%d, default=%s)" 
            (to_str ~array_to_str a) dim offset (ParNum.to_str default)

  | Slice (a, dim, start, stop) -> 
        Printf.sprintf "slice(%s, dim=%d, start=%d, stop=%d)"
            (to_str ~array_to_str a)  dim start stop 
  | Range (start, stop) -> 
        Printf.sprintf "range(from=%d, to=%d)" start stop 

let rec map (f: 'a -> 'b) (x : 'a t) : 'b t = match x with 
  | Array (a, t, s) -> Array (f a, t, s) 
  | Nested elts -> Nested (Array.map (map f) elts) 
  | Rotate (a, dim, offset) -> Rotate (map f a, dim, offset)
  | Shift (a, dim, offset, default) -> Shift (map f a, dim, offset, default) 
  | Slice (a, dim, start, stop) -> Slice (map f a, dim, start, stop)
  | Scalar n -> Scalar n 
  | Range (start, stop) -> Range (start, stop)  
  | Explode (n, s) -> Explode (n,s ) 
  
    
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

let rec get_type = function 
  | Array(_,  elt_t, s) -> Type.ArrayT (elt_t, Shape.rank s)
  | Nested _ -> failwith "nested arrays not supported"
  | Scalar n -> Type.ScalarT (ParNum.type_of n) 
  | Explode (n, s) -> Type.ArrayT (ParNum.type_of n, Shape.rank s) 
  | Shift (x, _, _, _)
  | Slice (x, _, _, _) 
  | Rotate (x, _, _) -> get_type x  
  | Range _ -> Type.ArrayT(Type.Int32T, 1)

let rec get_shape _ = Shape.of_list []
