
type scalar_op =
  | Add | Sub | Mult | Div | Pow | Log 
  | Mod  | Min | Max  
  | Eq | Neq  | Lt | Lte | Gt | Gte
  | And| Or | Not   
  | Neg 
  | Sqrt 
  | Sign 
  | Reciprocal 
  | Abs 
  | Ceil | Floor | Round  
  | Exp | Exp2
  | Sin | Cos | Tan | Sinh | Cosh | Tanh 
  | Ln | Lg2 | Log10 
  | Select
      (* returns array where i'th element is from 2nd arg if 
         i'th conditional element is true, otherwise choose from 3rd arg
      *) 


type array_op = 
  | Map 
  | EachLeft (* each left *)
  | EachRight (* each right *)
  | Scan  
  | Reduce 
  | AllPairs (* each left / each right *) 
  | AllPairsRight  (* each right / each left *)
  | Zip 
  | Concat 
  | Enlist (* make array from one element *) 
  | Til 
  | Rand
  | Index

type impure_op = ResetTimer | GetTimer | Print 
                

type q_op =  Q_WriteOrLoadText | Q_WriteOrLoadBinary | Q_Rand |  Q_Dollar  
      

type prim =
  | ScalarOp of scalar_op  
  | ArrayOp of array_op
  | ImpureOp of impure_op
  | Q_Op of q_op 

    
        
let is_adverb = function 
  | Map 
  | EachLeft (* each left *)
  | EachRight (* each right *)
  | Scan  
  | Reduce 
  | AllPairs (* each left / each right *) 
  | AllPairsRight  (* each right / each left *) -> true
  | _ -> false
  
let is_binop = function  
  | Add 
  | Sub 
  | Mult 
  | Div 
  | Pow 
  | Log 
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
  | Lg2 
  | Log10 -> true
  | _ -> false 

(* does the operator expect a function argument *) 
let is_higher_order = function 
  | Map 
  | EachLeft (* each left *)
  | EachRight (* each right *)
  | Scan  
  | Reduce 
  | AllPairs  
  | AllPairsRight  -> true 
  | _ -> false 


(* binary operators which preserve the greatest type of their args *) 
let is_comparison = function   
  | Eq | Neq | Lt  | Lte | Gt  | Gte -> true 
  | _ -> false

(* unary operators which turn numeric types into floats *)
let is_float_unop = function 
  | Sqrt  | Lg2 | Ln | Log10 | Cos | Sin | Tan | Cosh | Sinh | Tanh 
  | Reciprocal | Exp -> true 
  | _ -> false  


 
module PrimOrd = struct type t = prim let compare = compare end
module PrimSet = Set.Make(PrimOrd)
module PrimMap = Map.Make(PrimOrd)

let min_prim_arity = function
  | ScalarOp op when is_binop op -> 2
  | ScalarOp Select -> 3  
  | ScalarOp _ -> 1   
  | ArrayOp Til
  | ArrayOp Enlist -> 1
  | ArrayOp Concat 
  | ArrayOp Zip
  | ArrayOp Rand -> 2
  | ArrayOp Index -> 2 
  | ArrayOp _ -> 3
  | ImpureOp Print  -> 1
  | ImpureOp _ -> 0
  | Q_Op _ -> 2 

let max_prim_arity = function 
  | ArrayOp Map
  | ArrayOp Concat 
  | ArrayOp Reduce 
  | ArrayOp Zip -> max_int 
  | other -> min_prim_arity other
	
let scalar_op_to_str = function 
	| Add -> "+"
	| Sub -> "-"
	| Mult -> "*"
	| Div -> "%"
	| Mod -> "mod"
	| Min -> "min"
	| Max -> "max"
	| Pow -> "pow" 
	| Log -> "log"
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
  | Lg2 -> "lg2"
  | Log10 -> "log10"
  | Select -> "?"
	

let array_op_to_str = function 
	| Zip -> "zip"
	| Concat -> ","
	| Enlist -> "enlist"
	| Til -> "til"
	| Rand -> "rand"
  | Map -> "map"
  | EachLeft -> "each_left"
  | EachRight -> "each_right"
  | Reduce -> "/"
  | Scan -> "\\"
  | AllPairs -> "/:\\:" 
  | AllPairsRight -> "\\:/:"  
  | Index -> "@"

let impure_op_to_str = function
  | ResetTimer -> "reset_timer"
  | GetTimer -> "get_timer"
  | Print -> "print"

let q_op_to_str = function 
  | Q_WriteOrLoadText -> "0:"
  | Q_WriteOrLoadBinary -> "1:"
  | Q_Rand -> "?"
  | Q_Dollar -> "$"
 
let prim_to_str = function 
  | ScalarOp op -> scalar_op_to_str op 
  | ArrayOp op ->  array_op_to_str op
  | ImpureOp op -> impure_op_to_str op   
  | Q_Op op -> q_op_to_str op 
  
let is_pure_op  = function 
  | Q_Op op -> op = Q_Dollar    
  | ImpureOp _ -> false
  | _ -> true  
