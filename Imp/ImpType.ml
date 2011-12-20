
type elt_t = Type.elt_t
type t =
    | ScalarT of elt_t
    | ArrayT of elt_t * int
    | ShiftT of t

let rec to_str = function
	| ScalarT elt_t -> Type.elt_to_str elt_t
	| ArrayT (elt_t, r) ->
      Printf.sprintf "array(%s, %d)" (Type.elt_to_str elt_t) r
	| ShiftT t -> Printf.sprintf "shift(%s)" (to_str t)

let rec elt_type = function
	| ScalarT t -> t
  | ArrayT (t, _) -> t 
  | ShiftT nested -> elt_type nested 
  
let is_scalar = function
	| ScalarT _ -> true
	| _ -> false

let is_array = function 
  | ArrayT _ -> true
  | _ -> false 

let rec rank = function
	| ScalarT _ -> 0
	| ArrayT (_, r) -> r
	| ShiftT x -> rank x

let bool_t = ScalarT Type.BoolT
let char_t = ScalarT Type.CharT
let int16_t = ScalarT Type.Int16T
let int32_t = ScalarT Type.Int32T
let int64_t = ScalarT Type.Int64T
let float32_t = ScalarT Type.Float32T
let float64_t = ScalarT Type.Float64T

let common_type t1 t2 = match t1, t2 with 
  | ScalarT t1', ScalarT t2' -> ScalarT (Type.common_elt_type t1' t2')
  | _ -> if t1 = t2 then t1 else 
         failwith ("Can't unify imp types " ^ (to_str t1) ^ " and " ^ (to_str t2)) 
        
let rec combine_type_list = function 
  | [] -> failwith "Can't combine empty type list"
  | [t] -> t
  | t::ts -> common_type t (combine_type_list ts)  