open Base
open Printf 

type elt_t = 
  | BoolT
  | CharT
  | Int16T
  | Int32T
  | Int64T
  | Float32T
  | Float64T 

type t  =
  | ScalarT of elt_t 
  | ArrayT of elt_t * int
  | BottomT (* bottom of type lattice for vars whose type is not determined *)
  | AnyT (* top of type lattice, for variables whose type is overdetermined *)
 
let bool = ScalarT BoolT
let char = ScalarT CharT
let int16 = ScalarT Int16T 
let int32 = ScalarT Int32T 
let int64 = ScalarT Int64T 
let float32 = ScalarT Float32T
let float64 = ScalarT Float64T 

let elt_to_str = function 
  | BoolT -> "bool"
  | CharT -> "char"
  | Int16T -> "int16"
  | Int32T -> "int32"
  | Int64T -> "int64"
  | Float32T -> "float32"
  | Float64T -> "float64"
   
let to_str = function 
  | BottomT -> "bottom"
  | AnyT -> "any"
  | ScalarT t -> (elt_to_str t)
  | ArrayT (eltT, d) -> (string_of_int d) ^ "D array of " ^ (elt_to_str eltT)
 
let type_list_to_str ts = String.concat ", " (List.map to_str ts)
let type_array_to_str ts = type_list_to_str (Array.to_list ts)
 

let sizeof = function
  | CharT
  | BoolT -> 1
  | Int16T -> 2  
  | Int32T 
  | Float32T -> 4
  | Int64T 
  | Float64T -> 8

let elt_is_integer = function 
  | BoolT 
  | CharT 
  | Int16T
  | Int32T 
  | Int64T -> true  
  | _ -> false

let is_integer = function 
  | ScalarT t -> elt_is_integer t 
  | _ -> false 

let elt_is_floating = function 
  | Float32T
  | Float64T -> true
  | _ -> false  

let is_floating = function 
  | ScalarT t -> elt_is_floating t 
  | _ -> false 

let elt_is_number t = elt_is_integer t || elt_is_floating t 
let is_number t = is_integer t || is_floating t

let is_scalar = function 
  | ScalarT _ -> true
  | _ -> false  

let is_compound t = not (is_scalar t)

let is_array = function
    | ArrayT _ -> true
    | _ -> false
 
let is_num_or_array = function
  | ScalarT _ 
  | ArrayT _  -> true
  | _ -> false 
  
let is_numarray = function
    |  ArrayT (t, _) when elt_is_number t -> true
    | _ -> false


let elt_type = function
  | ArrayT (t, _) -> t
  | ScalarT t -> t
  | t -> failwith ("Can't get elt type of " ^ (to_str t))

let rec fill_elt_type t e = match t with   
  | ArrayT (_, r) -> ArrayT(e, r)
  | ScalarT _ -> ScalarT e 
  | other -> other   


(* reduce the dimensionality of an array by 1 *)  
(* TODO: ACTUALLY USE AXES! *) 
let peel ?(num_axes=1) = function 
  | ArrayT(eltT, d) -> 
        if d <= num_axes then ScalarT eltT 
        else ArrayT(eltT, d-num_axes)  
  | ScalarT t -> ScalarT t  
  | t -> failwith
     (Printf.sprintf 
        "[peel] expected array or scalar, got : %s"
        (to_str t)
     )

let is_scalar_subtype s1 s2 =
    (s1 = s2) ||   
    (sizeof s1 < sizeof s2)  || 
    (elt_is_integer s1 && elt_is_floating s2) 
   
(* how deep is the nesting of vectors in a given type *)
let rank = function
  | ArrayT (_, d) -> d
  | _ -> 0
 
(*
  VecT IntT and VecT FloatT has equivalent structure, whereas 
  VecT(VecT Int) and VecT IntT don't
*)
let equiv_type_structure t1 t2 = (rank t1 = rank t2) 
  
(* VecT IntT is a structure subtype of VecT (VecT IntT) *)
(* BUT: VecT IntT is *not* a structure subtype of IntT *)
let rec is_structure_subtype t1 t2 = (rank t1 <= rank t2) 
  
(*
  If t1 is a subtype of t2, returns how nested within t2 is t1.
  Otherwise returns None.
*)
let rec relative_rank t1 t2 =
  let diff = rank t1 - rank t2 in 
  if diff >= 0 then Some diff
  else None

let common_elt_type t1 t2 = 
  if t1 = t2 then t1 
  else if is_scalar_subtype t1 t2 then t2 
  else t1

let rec common_elt_type_list = function 
  | [] -> BoolT 
  | t::ts -> common_elt_type t (common_elt_type_list ts)  

let common_type t1 t2  =
	if t1 = t2 then t1
  else match t1, t2 with 
    | ScalarT s1, ScalarT s2 -> 
      if is_scalar_subtype s1 s2 then t2
      else if is_scalar_subtype s2 s1 then t1 
      else AnyT
    | ArrayT (s2, d), ScalarT s1 
    | ScalarT s1, ArrayT(s2, d) -> 
      if is_scalar_subtype s1 s2 then ArrayT(s2,d) else AnyT 
    | ArrayT(s1, d1), ArrayT(s2, d2) -> 
      if is_scalar_subtype s1 s2 && d1 <= d2 then t2
      else if is_scalar_subtype s2 s1 && d2 <= d1 then t1
      else AnyT 
    | BottomT, _ -> t2
    | _, BottomT -> t1
    | _ -> AnyT   
    
let combine_type_array arr =
  if Array.length arr < 1 then AnyT 
  else 
     let base_t = arr.(0) in
     Array.fold_left common_type base_t arr

let combine_type_list = function
  | [] -> AnyT 
  | t::ts -> List.fold_left common_type t ts




let replace_elt_type t s = match t with  
  | ArrayT (_, d) -> ArrayT(s,d)
  | ScalarT _ -> ScalarT s 
  | _ -> t  

let increase_rank r = function 
  | ArrayT (elt_t, old_r) -> ArrayT (elt_t, old_r + r)
  | ScalarT elt_t -> ArrayT (elt_t, r)
  | other -> failwith ("[Type] Can't increase rank of type " ^ (to_str other))

let increase_ranks r ts = List.map (increase_rank r) ts 

let maximal_type types =
  let rec aux highestRank currT = function
    | [] -> currT
    | t::ts ->
      let r = rank t in
      if r >= highestRank then aux r t ts 
      else aux highestRank currT ts 
  in aux 0 BottomT types  
  

(* only peel types of maximal depth *)           
let rec peel_maximal types = 
  let ranks = List.map rank types in 
  let maxDepth = List.fold_left max 0 ranks in 
  List.map2 
    (fun t depth -> if depth = maxDepth then peel t else t)   
    types 
    ranks   