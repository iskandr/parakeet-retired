open Base
open Prim

let prim_names = Hashtbl.of_list [
  "add", ScalarOp Add;
  "sub", ScalarOp Sub;
  "div", ScalarOp Div;
  "mult", ScalarOp Mult;
  "mod", ScalarOp Mod;
  "max", ScalarOp Max;
  "min", ScalarOp Min;

  "eq", ScalarOp Eq;
  "neq", ScalarOp Neq;
  "lt", ScalarOp Lt;
  "lte", ScalarOp Lte;
  "gt", ScalarOp Gt;
  "gte", ScalarOp Gte;

  "sqrt", ScalarOp Sqrt;
  "log", ScalarOp Ln;
  "exp", ScalarOp Exp;
  "pow", ScalarOp Pow;

  "not", ScalarOp Not;
  "neg", ScalarOp Neg;
  "sign", ScalarOp Sign;

  "and", ScalarOp And;
  "or", ScalarOp Or;

  "reciprocal", ScalarOp Reciprocal;
  "floor", ScalarOp Floor;
  "ceil", ScalarOp Ceil;
  "abs", ScalarOp Abs;

  "select", ScalarOp Select;
  "map", Adverb Map;
  "allpairs", Adverb AllPairs;
  "scan", Adverb Scan;
  "reduce", Adverb Reduce;

  "index",  ArrayOp Index;
  "slice", ArrayOp Slice;
  "range", ArrayOp Range;
  "size", ArrayOp DimSize;
]

let get_prim str = match Hashtbl.find_option prim_names str with
  | Some prim -> AST_Helpers.mk_prim_node prim
  | None ->
    if FnManager.have_untyped_function str then AST_Helpers.mk_var_node str
    else failwith $ "Couldn't find Parakeet primitive named " ^ str
