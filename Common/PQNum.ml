open Base 
open DynType

type num = 
  | Bool of bool 
  | Char of char 
  | UInt16 of int 
  | Int16 of int 
  | UInt32 of Uint64.t
  | Int32 of Int32.t
  | UInt64 of Uint64.t   
  | Int64 of Int64.t
  | Float32 of float
  | Float64 of float
  | Inf of DynType.t
  | NegInf of DynType.t 

let num_to_str = function
  | UInt16 x  
  | Int16 x -> string_of_int x
  | Int32 x -> Int32.to_string x
  | Int64 x -> Int64.to_string x
  | UInt32 ui 
  | UInt64 ui -> Uint64.to_string ui 
  | Float32 x
  | Float64 x -> Float.to_string x
  | Bool x -> Int.to_string (Bool.to_int x)
  | Char c ->  Char.to_string c
  | Inf t -> Printf.sprintf "inf : %s" (DynType.to_str t)
  | NegInf t -> Printf.sprintf "-inf : %s" (DynType.to_str t)  

let type_of_num = function 
  | UInt16 _ -> UInt16T 
  | Int16 _ -> Int16T 
  | UInt32 _ -> UInt32T 
  | Int32 _ -> Int32T
  | UInt64 _ -> UInt64T 
  | Int64 _ -> Int64T  
	| Float32 _ -> Float32T
  | Float64 _ -> Float64T 
	| Bool _ -> BoolT
  | Char _ -> CharT 
  | Inf t
  | NegInf t -> t 

let coerce_int i = function
  | UInt16T -> assert (i >= 0); UInt16 i 
  | Int16T -> Int16 i  
  | UInt32T -> UInt32 (Uint64.of_int i)  
  | Int32T -> Int32 (Int32.of_int i)
  | UInt64T -> UInt64 (Uint64.of_int i) 
  | Int64T -> Int64 (Int64.of_int i)
  | Float32T -> Float32 (float_of_int i)
  | Float64T -> Float64 (float_of_int i)
  | BoolT -> 
        if i < 0 then failwith "cannot convert negative integer to bool"
        else Bool (i > 0)
  | CharT -> 
        if i < 0 || i > 255 
        then failwith "int outside valid range for conversion to char"
        else Char (Char.chr i)
  | t -> failwith $ Printf.sprintf 
         "coercion from int to %s not implemented"
         (DynType.to_str t)

let coerce_int32 i = function
  | UInt32T 
  | Int32T -> Int32 i
  | UInt64T  
  | Int64T -> Int64 (Int64.of_int32 i)
  | Float32T -> Float32 (Int32.to_float i)
  | Float64T -> Float64 (Int32.to_float i)
  | BoolT -> 
        if i < Int32.zero 
        then failwith "cannot convert negative integer to bool"
        else Bool (i > Int32.zero) 
  | CharT -> 
        if i < Int32.zero || i > Int32.zero 
        then failwith "int32 outside valid range for conversion to char"
        else Char (Char.chr (Int32.to_int i))
  | t -> failwith $ Printf.sprintf  
           "coercion from int32 to %s not implemented"
           (DynType.to_str t)

(* this is really an argument for a common NUMBER module interface *)
(* which is implemented by all of the ocaml number types *) 
let coerce_int64 i = function
  | UInt32T -> UInt32 (Uint64.of_int64 i)
  | Int32T -> Int32 (Int64.to_int32 i)
  | UInt64T -> UInt64 (Uint64.of_int64 i) 
  | Int64T -> Int64 i
  | Float32T -> Float32 (Int64.to_float i)
  | Float64T -> Float64 (Int64.to_float i)
  | BoolT -> 
        if i < Int64.zero 
        then failwith "cannot convert negative integer to bool"
        else Bool (i > Int64.zero) 
  | CharT -> 
        if i < Int64.zero || i > Int64.zero 
        then failwith "int64 outside valid range for conversion to char"
        else Char (Char.chr (Int64.to_int i))
  | t -> failwith $ Printf.sprintf 
           "coercion from int64 to %s not implemented"
           (DynType.to_str t)

let coerce_float f = function
  | UInt32T -> UInt32 (Uint64.of_float f)
  | Int32T -> Int32 (Int32.of_float f)
  | UInt64T  
  | Int64T -> Int64 (Int64.of_float f)
  | Float32T -> Float32 f
  | Float64T -> Float64 f
  | BoolT -> 
        if f < 0. 
        then failwith "cannot convert negative integer to bool"
        else Bool (f > 0.) 
  | CharT -> 
        if f < 0. || f > 255.  
        then failwith "float outside valid range for conversion to char"
        else Char (Char.chr (int_of_float f))
  | t -> failwith $ Printf.sprintf  
           "[PQNum] coercion from float to %s not implemented" 
           (DynType.to_str t)

         
let coerce_num n t =
  match n with 
    | Int16 i
    | UInt16 i -> coerce_int i t
    | Int32 i -> coerce_int32 i t
    | UInt32 unsigned
    | UInt64 unsigned -> coerce_int64 (Uint64.to_int64 unsigned) t  
    | Int64 i -> coerce_int64 i t 
    | Float32 f 
    | Float64 f -> coerce_float f t  
    | Bool b -> if b then coerce_int 1 t else coerce_int 0 t 
    | Char c -> coerce_int (Char.code c) t
    | Inf _ -> Inf t 
    | NegInf _ -> NegInf t  

let to_int = function
  | UInt16 i 
  | Int16 i -> i 
  | Int32 i -> Int32.to_int i
  | UInt32 ui  
  | UInt64 ui -> Uint64.to_int ui 
  | Int64 i -> Int64.to_int i  
  | Float32 f
  | Float64 f -> Float.to_int f  
  | Bool b -> if b then 1  else 0 
  | Char c -> Char.code c
  | Inf _ -> max_int
  | NegInf _ -> min_int

let to_int32 = function 
  | UInt16 i 
  | Int16 i  -> Int32.of_int i 
  | Int32 i32 -> i32
  | UInt32 ui
  | UInt64 ui -> Uint64.to_int32 ui 
  | Int64 i64 -> Int64.to_int32 i64 
  | Float32 f
  | Float64 f -> Int32.of_float f 
  | Bool b -> if b then Int32.one else Int32.zero
  | Char c -> Int32.of_int (Char.code c)
  | Inf _ -> Int32.max_int 
  | NegInf _ -> Int32.min_int


let to_float = function 
  | Int32 i -> Int32.to_float i
  | UInt32 ui  
  | UInt64 ui -> Uint64.to_float ui 
  | Int64 i -> Int64.to_float i
  | Float32 f
  | Float64 f -> f
  | Inf _ -> max_float
  | NegInf _ -> min_float 
  | other -> Int.to_float (to_int other)

let is_zero = function 
  | Int32 i32 -> i32 = Int32.zero
  | UInt32 ui   
  | UInt64 ui -> ui = Uint64.zero 
  | Int64 i64 -> i64 = Int64.zero 
  | Float32 f | Float64 f -> f = 0.0 
  | other -> to_int other = 0   

let is_one = function 
  | Int32 i32 -> i32 = Int32.one
  | UInt32 ui 
  | UInt64 ui -> ui = Uint64.one 
  | Int64 i64 -> i64 = Int64.one 
  | Float32 f | Float64 f -> f = 1.0 
  | other -> to_int other = 1  

let is_inf = function 
  | Inf _  
  | NegInf _ -> true
  | _ -> false
