(* pp: -parser o pa_macro.cmo *)

open Base


(*

(**********************************************************************)
(*           DSL for more compactly building small SSA functions      *)
(**********************************************************************)

let (<--) xs y = set (List.map get_id xs) y
let (@@) fn args = app fn args
let scalar_op (x : Prim.scalar_op)  = op (Prim.ScalarOp x)
let array_op (x : Prim.array_op) = op (Prim.ArrayOp x)
let impure_op (x : Prim.impure_op)  = op (Prim.ImpureOp x)

let print = impure_op Prim.Print

let plus = scalar_op Prim.Add
let minus = scalar_op Prim.Sub
let mul = scalar_op Prim.Mult
let div = scalar_op Prim.Div

let lt = scalar_op Prim.Lt
let lte = scalar_op Prim.Lte
let eq = scalar_op Prim.Eq

let zero = num (ParNum.Int32 0l)
let one = num (ParNum.Int32 1l)
let neg_one = num (ParNum.Int32 (-1l))
let trueVal = num (ParNum.Bool true)
let falseVal = num (ParNum.Bool false)

let inf = num (ParNum.Inf Type.Float32T)
let neginf = num (ParNum.NegInf Type.Float32T)

let select = op (Prim.ScalarOp Prim.Select)

let reduce = op  (Prim.Adverb Prim.Reduce)
let map = op (Prim.Adverb Prim.Map)
let allPairs = op (Prim.Adverb Prim.AllPairs)

(*let where = op (Prim.ArrayOp Prim.Where)*)
let index = op (Prim.ArrayOp Prim.Index)
(*let til = op (Prim.ArrayOp Prim.Til)*)
let find = op (Prim.ArrayOp Prim.Find)
let dimsize = op (Prim.ArrayOp Prim.DimSize)

let value x = exp $ Values [x]
let values xs = exp $ Values xs

let len x = dimsize @@ [x; zero]
let incr (x:ID.t) (y:value_node) = set [x] (plus @@ [y;one])
let set_int (x:ID.t) (y:Int32.t) =
  set [x] (vals_exp [Num (ParNum.Int32 y)])

(****************************************************************)
(*                 Phi-Node Helpers                             *)
(****************************************************************)

*)
