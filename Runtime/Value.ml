open Base 

type 'a array_info = {
  data : 'a;
  array_type : Type.t;
  elt_type : Type.elt_t;
  array_shape : Shape.t;
  array_strides : int array;
}

type 'a t =
  | Array of 'a array_info
  | Scalar of ParNum.t
  | Explode of ParNum.t * Shape.t           (* scalar, shape *)
  | Rotate of 'a t * int * int              (* array, dim, offset *)
  | Shift of 'a t * int * int * ParNum.t    (* array, dim, offset, default *)
  | Slice of 'a t * int * int * int         (* array, dim, start, end *)
  | Range of int * int * int                (* start, stop; step *)

(* since array data is polymorphic it's by default printed as the *)
(* totally uninformative string '<array>'. If you want something more*)
(* specific than that, you'll have to supply your own array printing *)
(* function. *) 
let rec to_str ?(array_to_str=(fun _ -> "<array>")) = function 
  | Scalar n -> ParNum.to_str n
  | Array array_info -> array_to_str array_info  
  (*| Nested elts -> 
    Printf.sprintf "[%s]" 
        (String.concat ", " (List.map to_str (Array.to_list elts)))
  *) 
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
  | Range (start, stop, step) ->
        Printf.sprintf "range(from=%d, to=%d, step=%d)" start stop step

let rec map (f: 'a -> 'b) (x : 'a t) : 'b t = match x with 
  | Array array_info -> Array {array_info with data = f array_info.data } 
  (*| Nested elts -> Nested (Array.map (map f) elts)*) 
  | Rotate (a, dim, offset) -> Rotate (map f a, dim, offset)
  | Shift (a, dim, offset, default) -> Shift (map f a, dim, offset, default) 
  | Slice (a, dim, start, stop) -> Slice (map f a, dim, start, stop)
  | Scalar n -> Scalar n
  | Range (start, stop, step) -> Range (start, stop, step)
  | Explode (n, s) -> Explode (n, s)

let rec type_of = function
  | Array {array_type} -> array_type
  (*| Nested _ -> failwith "nested arrays not supported"*)
  | Scalar n -> Type.ScalarT (ParNum.type_of n)
  | Explode (n, s) -> Type.ArrayT (ParNum.type_of n, Shape.rank s)
  | Shift (x, _, _, _)
  | Slice (x, _, _, _)
  | Rotate (x, _, _) -> type_of x  
  | Range _ -> Type.ArrayT(Type.Int32T, 1)

let rec shape_of _ = Shape.of_list []

let to_num = function 
  | Scalar n -> n 
  | other -> failwith $ Printf.sprintf
                 "Can't get scalar from interpreter value: %s"
                 (to_str other)

let to_bool x = ParNum.to_bool (to_num x)
let to_char x = ParNum.to_char (to_num x)        
let to_int x = ParNum.to_int (to_num x)
let to_int32 x = ParNum.to_int32 (to_num x)
let to_int64 x = ParNum.to_int64 (to_num x)
let to_float x = ParNum.to_float (to_num x)

let of_num n = Scalar n
let of_bool b = of_num (ParNum.of_bool b)
let of_char c = of_num (ParNum.of_char c)
let of_int i = of_num (ParNum.of_int i)
let of_float32 f = of_num (ParNum.of_float32 f)
let of_float f = of_num (ParNum.of_float f)
let of_int32 i32 = of_num (ParNum.of_int32 i32)
let of_int64 i64 = of_num (ParNum.of_int64 i64)
 
let mk_array (data:'a) (elt_t:Type.elt_t) (shape:Shape.t) (strides:int array) =
  let ty = Type.ArrayT(elt_t, Shape.rank shape) in  
  Array {
    data = data; 
    array_type = ty; 
    elt_type = elt_t; 
    array_shape = shape; 
    array_strides = strides; 
  }  

let is_scalar x = Type.is_scalar (type_of x)

let get_shape = function
  | Array {array_shape} -> array_shape
  | _ -> failwith "Cannot get shape of non-array"

let get_strides = function
  | Array {array_strides} -> array_strides
  | _ -> failwith "Cannot get strides of non-array"

let rec extract = function
	| Rotate (x, _, _)
	| Shift (x, _, _, _)
  | Slice (x, _, _, _) -> extract x
  | Array {data} -> Some data
	| Scalar _
  | Explode _
  | Range _ -> None

let rec collect_list = function
	| [] -> []
	| x::xs ->
			let rest = collect_list xs in
			(match extract x with None -> rest | Some d -> d :: rest)
