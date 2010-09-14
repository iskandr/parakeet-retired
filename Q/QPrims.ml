open Base 
open AST
open Prim
	
let prim_hash =  Hashtbl.from_list [
			"+", ScalarOp Add;
			"-", ScalarOp Sub; 
			"%", ScalarOp Div;
			"*", ScalarOp Mult; 
			"mod", ScalarOp Mod; 
			"|", ScalarOp Max;
			"&", ScalarOp Min; 
			"xexp", ScalarOp Pow; 
			"xlog", ScalarOp Log;
			
			"=", ScalarOp Eq; 
			"<>", ScalarOp Neq; 
			"<", ScalarOp Lt; 
			"<=", ScalarOp Lte;
			">", ScalarOp Gt; 
			">=", ScalarOp Gte; 
			
			"sqrt", ScalarOp Sqrt;
			"log", ScalarOp Ln;   
			"not", ScalarOp Not; 
			"neg", ScalarOp Neg; 
			"signum", ScalarOp Sign; 
			"reciprocal", ScalarOp Reciprocal;
			"floor", ScalarOp Floor; 
			"ceiling", ScalarOp Ceil;
			"abs", ScalarOp Abs;  
      "exp", ScalarOp Exp; 
			
      "??", ScalarOp Select;
      
			"'", ArrayOp Map; 
			"/:", ArrayOp EachLeft; 
			"\\:", ArrayOp EachRight;
			"each", ArrayOp Map; 
			"/:\\:", ArrayOp AllPairs;
			"\\:/:", ArrayOp AllPairs;
			"\\", ArrayOp Scan;
      "/", ArrayOp Reduce; 
      
      "enlist", ArrayOp Enlist; 
       
			"til", ArrayOp Til;
      ",", ArrayOp Concat; 
      (* the actual Q syntax is ?, but QSyntax_to_AST rewrites this *) 
      

      (* the degree of overloading in this language makes me want to 
         live in a cave and never use a computer again 
      *)       
      "?", Q_Op Q_Rand; 	

      "0:", Q_Op Q_WriteOrLoadText;
      "1:", Q_Op Q_WriteOrLoadBinary;
      "$", Q_Op Q_Dollar
		]	
		
let prim_or_var str =
	if Hashtbl.mem prim_hash str 
	then Prim (Hashtbl.find prim_hash str)
	else Var str   
 
