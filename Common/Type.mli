
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
  | ArraryT of elt_t * int
  | BottomT (* bottom of type lattice for vars whose type is not determined *)
  | AnyT (* top of type lattice, for variables whose type is overdetermined *)
 

val elt_to_str : elt_t -> string
val to_str : t -> string
 
val type_list_to_str : t list -> string
val type_array_to_str : t array -> string
 
val sizeof : elt_t -> int

val elt_is_integer : elt_t -> bool
val is_integer : t -> bool

val elt_is_floating : elt_t -> bool
val is_floating : t -> bool

val elt_is_number : elt_t ->  bool
val is_number : t -> bool
val is_scalar : t -> bool
val is_compound : t -> bool
val is_array : t -> bool
val is_num_or_array : t -> bool
val is_numarray : t -> bool

val elt_type : t -> elt_t 

val peel : ?num_axes:int -> t -> t
val is_scalar_subtype : elt_t -> elt_t -> bool
val rank : t -> int
val equiv_type_structure : t -> t -> bool 
val is_structure_subtype : t -> t -> bool  
  
val relative_rank : t -> t -> int option 
val common_type : t -> t -> t 

val combine_type_array : t array -> t 
val combine_type_list : t list -> t 

val replace_elt_type : t -> elt_t -> t

(*
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
*)      
(* only peel types of maximal depth *)           
val peel_maximal : t list -> t list
