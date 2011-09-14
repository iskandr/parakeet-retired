open Base
open Printf 

type t  =
  | BottomT (* bottom of type lattice for vars whose type is not determined *)
  | AnyT (* top of type lattice, for variables whose type is overdetermined *)
  | UnitT
  | BoolT
  | CharT
  | Int16T
  | UInt16T 
  | Int32T
  | UInt32T
  | Int64T
  | UInt64T
  | Float32T
  | Float64T
  | SymT (* symbol *) 
  | StrT
  | VecT of t
  | TupleT of t array
  | TableT of t String.Map.t
  (* use this to replace VecT, stores both element type and rank  *) 
  | ArrT of t * int   
            
  
let rec to_str = function 
  | BottomT -> "bottom"
  | UnitT  -> "unit"
  | CharT -> "char"
  | UInt16T -> "uint16"
  | Int16T -> "int16"
  | UInt32T -> "uint32"
  | Int32T -> "int32"
  | UInt64T -> "uint64"
  | Int64T -> "int64"
  | BoolT -> "bool"
  | Float32T -> "float32"
  | Float64T -> "float64"
  | SymT -> "sym"
  | StrT -> "str"
  | AnyT -> "any"
  | VecT t -> sprintf "vec[%s]" (to_str t)
  | TupleT ts -> 
    "(" ^ (String.concat " * " (Array.to_list (Array.map to_str ts))) ^ ")"
  | TableT _ -> "table"
  (*| FnT (x,y) -> 
      let sx = type_list_to_str x in
      let sy = type_list_to_str y in
      sprintf "%s -> %s"
        (if List.length x > 0 then "{" ^ sx ^ "}" else sx)
        (if List.length y > 0 then "{" ^ sy ^ "}" else sy)   
  | AnyFnT -> "? -> ?"
  | BottomFnT -> "bottom_fn"
  *) 
  | ArrT (t, rank) -> "arr("^ (to_str t) ^ ", " ^ (string_of_int rank) ^ ")"
and type_list_to_str ts = String.concat ", " (List.map to_str ts)

let type_array_to_str ts = type_list_to_str (Array.to_list ts)
 
(* number of bytes needed to represent a value of a given type *) 
exception UnknownSize of string  
let sizeof = function
  | CharT
  | BoolT -> 1
  | UInt16T
  | Int16T -> 2
  | SymT (* assume symbols are represented as 32-bit integers *)  
  | UInt32T 
  | Int32T 
  | Float32T -> 4
  | Int64T 
  | UInt64T
  | Float64T -> 8
  | t -> raise (UnknownSize (to_str t))

let is_integer = function 
  | BoolT 
  | CharT 
  | Int16T
  | UInt16T 
  | Int32T 
  | UInt32T
  | Int64T
  | UInt64T -> true 
  | _ -> false

let is_floating = function 
  | Float32T
  | Float64T -> true
  | _ -> false  

let is_number t = is_integer t || is_floating t 

let is_scalar t = is_number t || t = UnitT || t = SymT

let is_compound t = not (is_scalar t)

let is_vec = function
  | ArrT _ 
  | VecT _ -> true
  | _ -> false

(* are you a scalar or a vector of scalars? *) 
let is_scalar_or_vec = function 
  | VecT t 
  | ArrT (t, _) -> is_scalar t 
  | t -> is_scalar t 

let is_numvec = function
    |  VecT t when is_number t -> true
    | _ -> false

(*
(* doesn't cover functions contained in vectors or anything like that *) 
let is_function = function 
  | FnT _ -> true
  | _ -> false 
*)

let rec elt_type = function
  | VecT t -> elt_type t
  | ArrT (t, _) -> t 
  | t -> t

(* given a type "vec t" returns t *) 
let peel_vec = function 
  | VecT t  
  | ArrT (t, _) -> t 
  | t when is_scalar t -> t 
  | t -> failwith
     (Printf.sprintf 
        "[peel_vec] expected vector or scalar, got : %s"
        (to_str t)
     )

(* July 4, 2010 (alexr): removed 's1 = s2' from last condition-- *)
(* want Int64 to be a subtype of Float32 *)  
let is_scalar_subtype t1 t2 =
  is_number t1 && is_number t2 && 
  let s1,s2 = sizeof t1, sizeof t2 in 
  (s1 < s2  || (is_integer t1 && is_floating t2))  
 
(*
  VecT IntT and VecT FloatT has equivalent structure, whereas 
  VecT(VecT Int) and VecT IntT don't
*)
let rec equiv_type_structure t1 t2 =
  match t1, t2 with
    | VecT t1', VecT t2' -> equiv_type_structure t1' t2'
    | _ -> is_scalar t1 && is_scalar t2

(* VecT IntT is a structure subtype of VecT (VecT IntT) *)
(* BUT: VecT IntT is *not* a structure subtype of IntT *)
let rec is_structure_subtype t1 t2 =
  match t1, t2 with
    | VecT t1', VecT t2' -> is_structure_subtype t1' t2'
    | _, VecT _ when is_scalar t1 -> true
    | _ -> is_scalar t1 && is_scalar t2

(* how deep is the nesting of vectors in a given type *)
let rec nest_depth = function
  | VecT t' -> 1 + (nest_depth t')
  | _ -> 0

(*
  If t1 is a subtype of t2, returns how nested within t2 is t1.
  Otherwise returns None.
*)
let rec relative_nest_depth t1 t2 =
  if not $ is_structure_subtype t1 t2 then None
  else
  let d1 = nest_depth t1 in
  let d2 = nest_depth t2 in
  Some (d2 - d1)

let rec common_type t1 t2  =
	if t1 = t2 then t1
	else if is_scalar_subtype t1 t2 then t2
	else if is_scalar_subtype t2 t1 then t1
  else match t1, t2 with 
	| (VecT t1'), (VecT t2')
  (* upgrade scalars to vectors *) 
  | (VecT t1'), t2'
  | t1', (VecT t2') ->
			begin match common_type t1' t2' with
				| AnyT -> AnyT  
        | t3 -> VecT t3
			end 
  (* the signed/unsigned pairs should be resolved toward the next biggest *)
  (* number type. For 64-bit integers, we can't go any higher so we stick *)
  (* with 64-bit signed integers. *) 
  | UInt16T, Int16T
  | Int16T, UInt16T -> Int32T 
  | Int32T, UInt32T
  | UInt32T, Int32T -> Int64T  
  | Int64T, UInt64T
  | UInt64T, Int64T -> Int64T
  (* BottomT is the least element of the type lattice *)
  | BottomT, _ -> t2
  | _, BottomT -> t1  
  | _ -> AnyT

let common_type_folder t_opt t2 =
    match t_opt with
      | AnyT -> AnyT
      | t1 -> common_type t1 t2

let fold_type_array arr =
  if Array.length arr < 1 then AnyT 
  else 
     let base_t = arr.(0) in
     Array.fold_left common_type_folder base_t arr

let fold_type_list = function
  | [] -> AnyT 
  | t::ts -> List.fold_left common_type_folder t ts

 
let fn_input_types = function 
  | FnT(inputs, _) -> inputs
  | _ -> failwith "[DynType] fn_input_types: expected function"

let fn_output_types = function 
  | FnT ( _, outputs) -> outputs
  | _ -> failwith "[DynType] fn_output_types: expected function"

let fn_input_arity t = List.length (fn_input_types t)
let fn_output_arity t = List.length (fn_output_types t)

(* recursively traverse vector type until you get to a non-vector *)
(* and replace that with the given argument *) 
let rec replace_elt_type vecType newEltType = match vecType with 
  | VecT vecType' -> VecT (replace_elt_type vecType' newEltType)
  | _ -> newEltType 


(* what's the return type of multidimensional indexing or slicing *) 
let rec slice_type arrayType types = match arrayType, types with 
  (* if we're not slicing an array, just return the same type *)
  | _, [] -> arrayType
  | VecT nestedT, (VecT idxT)::rest when is_integer idxT -> 
      VecT (slice_type nestedT rest)
  | VecT nestedT, idxT::rest when is_integer idxT ->
      slice_type nestedT rest 
  | _ when is_scalar arrayType -> 
      failwith "[slice_type] indexing into scalar not allowed"
  | notVec, notIndices -> 
      failwith (Printf.sprintf "Can't index into %s with indices of type %s"
        (to_str notVec) (type_list_to_str notIndices))
        
(* only peel types of maximal depth *)           
let rec peel_maximal types = 
  let depths = List.map nest_depth types in 
  let maxDepth = List.fold_left max 0 depths in 
  List.map2 
    (fun t depth -> if depth = maxDepth then peel_vec t else t)   
    types 
    depths   