
open Base 
open AST
open Prim

let prim_names =  Hashtbl.of_list [
    "+", ScalarOp Add;
    "-", ScalarOp Sub; 
    "/", ScalarOp Div;
    "*", ScalarOp Mult; 
    "mod", ScalarOp Mod; 
    "max", ScalarOp Max;
    "min", ScalarOp Min; 
    
    "eq", ScalarOp Eq; 
    "neq", ScalarOp Neq; 
    "<", ScalarOp Lt; 
    "<=", ScalarOp Lte;
    ">", ScalarOp Gt; 
    ">=", ScalarOp Gte; 
            
    "sqrt", ScalarOp Sqrt;
    "ln", ScalarOp Ln;
    "exp", ScalarOp Exp;   
    "pow", ScalarOp Pow; 
    "log", ScalarOp Log;
    
    "not", ScalarOp Not; 
    "neg", ScalarOp Neg; 
    "sign", ScalarOp Sign; 
    
    "reciprocal", ScalarOp Reciprocal;
    "floor", ScalarOp Floor; 
    "ceil", ScalarOp Ceil;
    "abs", ScalarOp Abs;
      
    "select", ScalarOp Select;
    "map", Adverb Map; 
    "each-left", Adverb EachLeft; 
    "each-right", Adverb EachRight;
    "allpairs", Adverb AllPairs; 
    "scan", Adverb Scan;            
    "reduce", Adverb Reduce;
       
    "where", ArrayOp Where; 
    "enlist", ArrayOp Enlist; 
       
    "range", ArrayOp Til;
    "concat", ArrayOp Concat; 
]   
                
let get_prim str =
    try  
        AST.mk_prim_node (Hashtbl.find prim_names str)
    with _ -> (
      if InterpState.have_untyped_function StdLib.initState str then 
        AST.mk_var_node str
      else   
        failwith $ "Couldn't find Parakeet primitive named " ^ str
    )   