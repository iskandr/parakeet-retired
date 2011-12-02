



type basic_block
type compiled_fn 

type tenv = ImpType.t ID.Map.t

(* initialize a compiled function by providing its argument and return types *) 
val init_compiled_fn : inputs:ImpType.t list -> outputs:ImpType.t list -> compiled_fn


val compile_stmt : compiled_fn -> tenv -> Imp.stmt -> basic_block 

val compile_stmt_seq : compiled_fn -> tenv -> Imp.stmt list -> basic_block 
   
val compile_fn : Imp.fn -> compiled_fn 
  

