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
      (* returns array where i'th element is from 2nd arg if
         i'th conditional element is true, otherwise choose from 3rd arg
      *)

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

type impure_op = ResetTimer | GetTimer | Print

type t =
  | ScalarOp of scalar_op
  | ArrayOp of array_op
  | Adverb of Adverb.t
  | ImpureOp of impure_op

let is_binop = function
  | Add
  | Sub
  | Mult
  | Div
  | SafeDiv
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
  | Or -> true
  | _ -> false

let is_unop = function
  | Neg
  | Not
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
  | Log10 -> true
  | _ -> false

(* binary operators which preserve the greatest type of their args *)
let is_comparison = function
  | Eq | Neq | Lt | Lte | Gt | Gte -> true
  | _ -> false

(* unary operators which turn numeric types into floats *)
let is_float_unop = function
  | Sqrt | Log2 | Ln | Log10 | Cos | Sin | Tan | Cosh | Sinh | Tanh
  | Reciprocal | Exp -> true
  | _ -> false



let is_float_op op = (is_float_unop op)

module PrimOrd = struct
  type prim = t
  type t = prim
  let compare = compare
end
module PrimSet = Set.Make(PrimOrd)
module PrimMap = Map.Make(PrimOrd)

let min_prim_arity = function
  | ScalarOp op when is_binop op -> 2
  | ScalarOp Select -> 3
  | ScalarOp _ -> 1
  | ArrayOp ArgMin
  | ArrayOp ArgMax
  | ArrayOp Where
  | ArrayOp Range -> 1
  | ArrayOp DimSize
  | ArrayOp Find
  | ArrayOp Index -> 2
  | Adverb Adverb.AllPairs -> 3
  | Adverb _ -> 2
  | ArrayOp Slice -> 4
  | ImpureOp Print  -> 1
  | ImpureOp _ -> 0

let max_prim_arity = function
  | Adverb Adverb.Map
  | Adverb Adverb.Reduce -> max_int
  | other -> min_prim_arity other

let scalar_op_to_str = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | SafeDiv -> "safediv"
  | Mod -> "mod"
  | Min -> "min"
  | Max -> "max"
  | Pow -> "pow"
  | Eq -> "="
  | Neq -> "<>"
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="
  | And -> "&&"
  | Or -> "||"
  | Neg -> "neg"
  | Not -> "not"
  | Sqrt -> "sqrt"
  | Sign -> "sign"
  | Reciprocal -> "recip"
  | Abs -> "abs"
  | Ceil -> "ceil"
  | Floor -> "floor"
  | Round -> "round"
  | Exp2 -> "exp2"
  | Exp -> "exp"
  | Sin -> "sin"
  | Cos -> "cos"
  | Tan -> "tan"
  | Sinh -> "sinh"
  | Cosh -> "cosh"
  | Tanh -> "tanh"
  | Ln -> "log"
  | Log2 -> "log2"
  | Log10 -> "log10"
  | Select -> "select"

let array_op_to_str = function
  | Index -> "index"
  | DimSize -> "dimsize"
  | Find -> "find"
  | ArgMin -> "argmin"
  | ArgMax -> "argmax"
  | Slice -> "slice"
  | Where -> "where"
  | Range -> "range"
  | Transpose -> "transpose"
  | Shape -> "shape"
  | Strides -> "strides"
  | Flatten -> "flatten"
  | Copy -> "copy"


let impure_op_to_str = function
  | ResetTimer -> "reset_timer"
  | GetTimer -> "get_timer"
  | Print -> "print"

let to_str = function
  | ScalarOp op -> scalar_op_to_str op
  | ArrayOp op ->  array_op_to_str op
  | Adverb op -> Adverb.to_str op
  | ImpureOp op -> impure_op_to_str op

let is_pure_op  = function
  | ImpureOp _ -> false
  | _ -> true

