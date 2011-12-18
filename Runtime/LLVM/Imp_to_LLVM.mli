type fn_record

val create_llvm_Vars : ID.t list -> ID.t list -> ID.t list

val create_llvm_types: ImpType.t list -> ImpType.t list -> Llvm.lltype list

(* Returns function llvalue *)
val codegen_proto: ID.t list -> Llvm.lltype list -> string -> Llvm.llvalue

val create_entry_block_alloca : Llvm.llvalue -> (string, Llvm.lltype) Hashtbl.t -> Llvm.llvalue

val create_argument_allocas : Llvm.llvalue -> ID.t list -> Llvm.lltype list -> unit

val create_local_allocas : Llvm.llvalue -> ID.t list -> Llvm.lltype list -> unit

val init_compiled_fn : ID.t list -> Llvm.lltype list -> fn_record -> string ->
                       Llvm_executionengine.ExecutionEngine.t -> Llvm.llbasicblock
                      
                      

val compile_val : Imp.value_node -> Llvm.llvalue

val compile_expr : fn_record -> Imp.exp_node -> Llvm.llvalue

val compile_stmt_seq : fn_record -> Llvm.llbasicblock -> Imp.stmt list -> Llvm.llbasicblock

val compile_stmt : fn_record -> Llvm.llbasicblock -> Imp.stmt -> Llvm.llbasicblock

val compile_fn : Imp.fn -> Llvm.llbasicblock