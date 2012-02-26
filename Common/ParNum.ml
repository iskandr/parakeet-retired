open Base

type t =
  | Bool of bool
  | Char of char
  | Int16 of int
  | Int32 of Int32.t
  | Int64 of Int64.t
  | Float32 of float
  | Float64 of float


let is_bool = function Bool _ -> true | _ -> false
let is_char = function Char _ -> true | _ -> false
let is_int16 = function Int16 _ -> true | _ -> false
let is_int32 = function Int32 _ -> true | _ -> false
let is_int64 = function Int64 _ -> true | _ -> false
let is_float32 = function Float32 _ -> true | _ -> false
let is_float64 = function Float64 _ -> true | _ -> false


let is_int x = is_bool x || is_char x || is_int16 x || is_int32 x || is_int64 x
let is_float x = is_float32 x || is_float64 x


let to_str = function
  | Int16 x -> string_of_int x
  | Int32 x -> Int32.to_string x
  | Int64 x -> Int64.to_string x
  | Float32 x
  | Float64 x -> Float.to_string x
  | Bool x -> Int.to_string (Bool.to_int x)
  | Char c ->  Char.to_string c


let type_of = function
  | Int16 _ -> Type.Int16T
  | Int32 _ -> Type.Int32T
  | Int64 _ -> Type.Int64T
	| Float32 _ -> Type.Float32T
  | Float64 _ -> Type.Float64T
	| Bool _ -> Type.BoolT
  | Char _ -> Type.CharT

let coerce_bool b t =
  let code = if b then 1 else 0 in match t with
  | Type.BoolT -> Bool (code > 0)
  | Type.CharT -> Char (Char.chr code)
  | Type.Int16T -> Int16 code
  | Type.Int32T -> Int32 (Int32.of_int code)
  | Type.Int64T -> Int64 (Int64.of_int code)
  | Type.Float32T -> Float32 (float_of_int code)
  | Type.Float64T -> Float64 (float_of_int code)

let coerce_char c t =
  let code = Char.code c in match t with
  | Type.BoolT -> Bool (code > 0)
  | Type.CharT -> Char c
  | Type.Int16T -> Int16 code
  | Type.Int32T -> Int32 (Int32.of_int code)
  | Type.Int64T -> Int64 (Int64.of_int code)
  | Type.Float32T -> Float32 (float_of_int code)
  | Type.Float64T -> Float64 (float_of_int code)

let coerce_int i = function
  | Type.Int16T -> Int16 i
  | Type.Int32T -> Int32 (Int32.of_int i)
  | Type.Int64T -> Int64 (Int64.of_int i)
  | Type.Float32T -> Float32 (float_of_int i)
  | Type.Float64T -> Float64 (float_of_int i)
  | Type.BoolT ->
        if i < 0 then failwith "cannot convert negative integer to bool"
        else Bool (i > 0)
  | Type.CharT ->
        if i < 0 || i > 255
        then failwith "int outside valid range for conversion to char"
        else Char (Char.chr i)
  (*| t -> failwith $ Printf.sprintf
         "coercion from int to %s not implemented"
         (Type.elt_to_str t)
  *)

let coerce_int32 i = function
  | Type.Int32T -> Int32 i
  | Type.Int64T -> Int64 (Int64.of_int32 i)
  | Type.Float32T -> Float32 (Int32.to_float i)
  | Type.Float64T -> Float64 (Int32.to_float i)
  | Type.BoolT ->
        if i < Int32.zero
        then failwith "cannot convert negative integer to bool"
        else Bool (i > Int32.zero)
  | Type.CharT ->
        if i < Int32.zero || i > Int32.zero
        then failwith "int32 outside valid range for conversion to char"
        else Char (Char.chr (Int32.to_int i))
  | t -> failwith $ Printf.sprintf
           "coercion from int32 to %s not implemented"
           (Type.elt_to_str t)

(* this is really an argument for a common NUMBER module interface *)
(* which is implemented by all of the ocaml number types *)
let coerce_int64 i = function
  | Type.Int32T -> Int32 (Int64.to_int32 i)
  | Type.Int64T -> Int64 i
  | Type.Float32T -> Float32 (Int64.to_float i)
  | Type.Float64T -> Float64 (Int64.to_float i)
  | Type.BoolT ->
        if i < Int64.zero
        then failwith "cannot convert negative integer to bool"
        else Bool (i > Int64.zero)
  | Type.CharT ->
        if i < Int64.zero || i > Int64.zero
        then failwith "int64 outside valid range for conversion to char"
        else Char (Char.chr (Int64.to_int i))
  | t -> failwith $ Printf.sprintf
           "coercion from int64 to %s not implemented"
           (Type.elt_to_str t)

let coerce_float f = function
  | Type.Int32T -> Int32 (Int32.of_float f)
  | Type.Int64T -> Int64 (Int64.of_float f)
  | Type.Float32T -> Float32 f
  | Type.Float64T -> Float64 f
  | Type.BoolT ->
        if f < 0.
        then failwith "cannot convert negative integer to bool"
        else Bool (f > 0.)
  | Type.CharT ->
        if f < 0. || f > 255.
        then failwith "float outside valid range for conversion to char"
        else Char (Char.chr (int_of_float f))
  | t -> failwith $ Printf.sprintf
           "coercion from float to %s not implemented"
           (Type.elt_to_str t)

let coerce n t =
  match n with
    | Int16 i -> coerce_int i t
    | Int32 i -> coerce_int32 i t
    | Int64 i -> coerce_int64 i t
    | Float32 f
    | Float64 f -> coerce_float f t
    | Bool b -> if b then coerce_int 1 t else coerce_int 0 t
    | Char c -> coerce_int (Char.code c) t


let of_bool b = Bool b
let of_char c = Char c
let of_int i = coerce_int i Type.Int32T
let of_int32 i32 = Int32 i32
let of_int64 i64 = Int64 i64
let of_float32 f = Float32 f
let of_float f = Float64 f

let to_bool = function
  | Bool b -> b
  | Char c -> Char.code c > 0
  | Int16 i -> i > 0
  | _ -> assert false

let to_char = function
  | Bool b -> Char.chr (Bool.to_int b)
  | Char c -> c
  | Int16 i -> Char.chr i
  | _ -> assert false

let to_int = function
  | Int16 i -> i
  | Int32 i -> Int32.to_int i
  | Int64 i -> Int64.to_int i
  | Float32 f
  | Float64 f -> Float.to_int f
  | Bool b -> if b then 1  else 0
  | Char c -> Char.code c


let to_int32 = function
  | Int16 i  -> Int32.of_int i
  | Int32 i32 -> i32
  | Int64 i64 -> Int64.to_int32 i64
  | Float32 f
  | Float64 f -> Int32.of_float f
  | Bool b -> if b then Int32.one else Int32.zero
  | Char c -> Int32.of_int (Char.code c)


let to_int64 = function
  | Int64 i64 -> i64
  | Float32 f
  | Float64 f -> Int64.of_float f
  | other -> Int64.of_int32 (to_int32 other)

let to_float = function
  | Int32 i -> Int32.to_float i
  | Int64 i -> Int64.to_float i
  | Float32 f
  | Float64 f -> f

  | other -> Int.to_float (to_int other)

let is_zero = function
  | Int32 i32 -> i32 = Int32.zero
  | Int64 i64 -> i64 = Int64.zero
  | Float32 f | Float64 f -> f = 0.0
  | other -> to_int other = 0

let is_one = function
  | Int32 i32 -> i32 = Int32.one
  | Int64 i64 -> i64 = Int64.one
  | Float32 f | Float64 f -> f = 1.0
  | other -> to_int other = 1

let zero = function
  | Type.Float32T -> Float32 0.0
  | Type.Float64T -> Float64 0.0
  | Type.Int16T -> Int16 0
  | Type.Int32T -> Int32 Int32.zero
  | Type.Int64T -> Int64 Int64.zero
  | Type.CharT -> Char (Char.chr 0)
  | Type.BoolT -> Bool false


let one = function
  | Type.Float32T -> Float32 1.0
  | Type.Float64T -> Float64 1.0
  | Type.Int16T -> Int16 1
  | Type.Int32T -> Int32 Int32.one
  | Type.Int64T -> Int64 Int64.one
  | Type.CharT -> Char (Char.chr 1)
  | Type.BoolT -> Bool true
