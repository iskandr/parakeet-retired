open Base 

type ty =  
  | Pred 
  | F16 | F32 | F64 
  | U8 | U16 | U32 | U64 | S8 | S16 | S32 | S64 
  | B8 | B16 | B32 | B64  
  | V2 of ty
  | V4 of ty

(* type of pointers on the gpu *) 
let ptrT = U64

(* size of a ptx type in bytes *)
let rec nbytes = function
  | Pred -> failwith "[ptx_nbytes] predicates don't have a size"
  | B8 | U8 | S8 -> 1 
  | B16 | U16 | S16 | F16 -> 2
  | B32 | U32 | S32 | F32 -> 4
  | B64 | S64 | U64 | F64 -> 8
  | V2 t -> 2 * (nbytes t)
  | V4 t -> 4 * (nbytes t)

let larger_type = function 
  | Pred | V2 _ | V4 _ | B64 | U64 | S64 | F64 -> failwith "no larger type"
  | B8 -> B16
  | B16 -> B32
  | B32 -> B64
  | F16 -> F32
  | F32 -> F64
  | U8 -> U16
  | U16 -> U32
  | U32 -> U64
  | S8 -> S16
  | S16 -> S32
  | S32 -> S64 

let is_float = function
  | F16 | F32 | F64 -> true
  | _ -> false

let is_int = function
  | U8 | S8 | U16 | S16 | U32 | S32 | U64 | S64 -> true
  | _ -> false

let is_signed = function 
  | S8 | S16 | S32 | S64 | F16 | F32 | F64 -> true
  | _ -> false 

let is_unsigned t = not $ is_signed t

let is_bits = function
  | B8 | B16 | B32 | B64 -> true
  | _ -> false


(* convert scalars to their corresponding GPU type and all compound
   types to a pointer. This is the type used after data is loaded from an 
   array. 
*)
let convert_elt_type = function 
  | Type.Int16T -> S16
  | Type.Int32T -> S32
  | Type.Int64T -> S64
  | Type.CharT -> U16 
  | Type.Float32T ->  F32 
  | Type.Float64T ->  F64
  | Type.BoolT -> Pred
  
let rec convert_type = function
  | Type.ScalarT t -> convert_elt_type t 
  | _ -> ptrT (* everything else is a pointer *)

(* the type of a variable when its stored in an array *) 
let storage_of_elt_type = function
  | Type.Int16T -> S16
  | Type.Int32T -> S32
  | Type.Int64T -> S64
  | Type.CharT -> U8
  | Type.Float32T ->  F32 
  | Type.Float64T ->  F64
  (* storing bools as 16-bit for now *) 
  | Type.BoolT -> U8

let to_storage_type = function 
  | Type.ScalarT eltT -> storage_of_elt_type eltT
  | _ -> ptrT  
 
let rec to_str = function 
  | Pred -> "pred"
  | F16 -> "f16"
  | F32 -> "f32"
  | F64 ->  "f64"
  | U8 -> "u8"
  | U16 -> "u16"
  | U32 -> "u32"
  | U64 -> "u64"
  | S8 -> "s8"
  | S16 -> "s16"
  | S32 -> "s32"
  | S64 -> "s64" 
  | B8 -> "b8"
  | B16 -> "b16"
  | B32 -> "b32"
  | B64 -> "b64"
  | V2 t -> "v2." ^ (to_str t)
  | V4 t -> "v4." ^ (to_str t)

let rec suffix = function 
  | U8 -> "c" | U16 -> "h" | U32 -> "u" | U64 -> "l"
  | S8 -> "sc" | S16 -> "sh" | S32 -> "s" | S64 -> "sl"
  | F16 -> "fh" | F32 -> "f" | F64 -> "d"
  | B8 -> "b" | B16 -> "bh" | B32 -> "b" | B64 -> "bl"
  | Pred -> "p"
  | V2 t -> (suffix t) ^ "_v2" 
  | V4 t -> (suffix t) ^ "_v4"
