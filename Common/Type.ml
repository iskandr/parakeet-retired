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
  | BottomT (* bottom of type lattice for vars whose type is not determined *)
  | AnyT (* top of type lattice, for variables whose type is overdetermined *)
  | ScalarT of elt_t 
  | ArrayT of elt_t * int 

let elt_to_str = function 
  | CharT -> "char"
  | Int16T -> "int16"
  | Int32T -> "int32"
  | Int64T -> "int64"
  | BoolT -> "bool"
  | Float32T -> "float32"
  | Float64T -> "float64"
   
let rec to_str = function 
  | BottomT -> "bottom"
  | AnyT -> "any"
  | ScalarT t -> (elt_to_str t)
  | ArrayT (eltT, d) -> (string_of_int d) ^ "D array of " ^ (elt_to_str eltT)
 
and type_list_to_str ts = String.concat ", " (List.map to_str ts)

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

(* reduce the dimensionality of an array by 1 *)  
let peel = function 
  | ArrayT(eltT, d) -> 
        if d <= 1 then ScalarT eltT 
        else ArrayT(eltT, d-1)  
  | ScalarT t -> ScalarT t  
  | t -> failwith
     (Printf.sprintf 
        "[peel] expected array or scalar, got : %s"
        (to_str t)
     )

let is_scalar_subtype t1 t2 =
  match t1, t2 with 
    | ScalarT s1, ScalarT s2 ->
        (s1 = s2) ||   
        (sizeof s1 < sizeof s2)  || 
        (elt_is_integer s1 && elt_is_floating s2)  
    | _ -> false 
   
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
let rec relative_nest_depth t1 t2 =
  if is_structure_subtype t1 t2 then Some (rank t1 - rank t2)
  else None 
  
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