open Base 
open DynType

type num = 
  | Int of int 
  | Int32 of Int32.t 
  | Int64 of Int64.t 
  | Float32 of float
  | Float64 of float
  | Char of char 
  | Bool of bool 

let num_to_str = function 
  | Int32 x -> Int32.to_string x
  | Int64 x -> Int64.to_string x
  | Int x -> Int.to_string x
  | Float32 x 
  | Float64 x -> Float.to_string x
  | Bool x -> Int.to_string (Bool.to_int x)
  | Char c ->  Char.to_string c  

let type_of_num = function 
  | Int32 _ -> Int32T
  | Int64 _ -> Int64T  
  | Int _ -> IntT
	| Float32 _ -> Float32T
  | Float64 _ -> Float64T 
	| Bool _ -> BoolT
  | Char _ -> CharT 


let coerce_int i = function
  | Int32T -> Int32 (Int32.of_int i)
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
  | IntT -> Int i 
  | _ -> failwith "coercion for this type not implemented"

let coerce_int32 i = function
  | Int32T -> Int32 i 
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
  | IntT -> Int (Int32.to_int i) 
  | _ -> failwith "coercion for this type not implemented"

(* this is really an argument for a common NUMBER module interface *)
(* which is implemented by all of the ocaml number types *) 
let coerce_int64 i = function
  | IntT -> Int (Int64.to_int i)
  | Int32T -> Int32 (Int64.to_int32 i) 
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
  | _ -> failwith "coercion for this type not implemented"

let coerce_float f = function
  | IntT -> Int (int_of_float f)
  | Int32T -> Int32 (Int32.of_float f) 
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
  | _ -> failwith "coercion for this type not implemented"


let coerce_num n t =
  match n with 
    | Int i -> coerce_int i t 
    | Int32 i -> coerce_int32 i t 
    | Int64 i -> coerce_int64 i t 
    | Float32 f 
    | Float64 f -> coerce_float f t  
    | Bool b -> if b then coerce_int 1 t else coerce_int 0 t 
    | Char c -> coerce_int (Char.code c) t