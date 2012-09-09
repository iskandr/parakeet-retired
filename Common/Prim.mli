type scalar_op =
  | Add
  | Sub
  | Mult
  | SafeDiv
  | Div
  | Pow
  | Mod
  | Min
  | Max
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  | And
  | Or
  | Not
  | Neg
  | Sqrt
  | Sign
  | Reciprocal
  | Abs
  | Ceil
  | Floor
  | Round
  | Exp
  | Exp2
  | Sin
  | Cos
  | Tan
  | Sinh
  | Cosh
  | Tanh
  | Ln
  | Log2
  | Log10
  | Select
      
type array_op =
  | Index
  | Find
  | Where
  | DimSize
  | ArgMin
  | ArgMax
  | Slice
  | Range
  | Transpose
  | Shape
  | Strides
  | Flatten
  | Copy

type adverb = 
  | Map 
  | Reduce  
  | Scan  
  | AllPairs  
 
type impure_op = ResetTimer | GetTimer | Print

type t =
  | ScalarOp of scalar_op
  | ArrayOp of array_op
  | Adverb of adverb 
  | ImpureOp of impure_op

val is_binop : scalar_op -> bool 

val is_unop : scalar_op -> bool 


val is_comparison : scalar_op -> bool

val is_float_unop : scalar_op -> bool 
  
val is_float_op : scalar_op -> bool 

module PrimOrd : sig 
  type prim = t
  type t = prim
  val compare : t -> t -> int 
end
module PrimSet : module type of Set.Make(PrimOrd)
module PrimMap : module type of Map.Make(PrimOrd)

val min_prim_arity : t -> int
val max_prim_arity : t -> int

val scalar_op_to_str : scalar_op -> string
val array_op_to_str : array_op -> string
val adverb_to_str : adverb -> string 

val impure_op_to_str : impure_op -> string
val to_str : t -> string
val is_pure_op : t -> bool
