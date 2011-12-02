



let compile_fn fn = 
  let inputTypes = List.map (fun id -> ID.Map.find id fn.types) fn.input_ids in
  let outputTypes = List.map (fun id -> ID.Map.find id fn.types) fn.output_ids in  
  let llvm_fn = init_compiled_fn inputTypes outputTypes in 
  let first_basic_block = compile_stmt_seq llvm_fn fn.types fn.body in
  (* TODO: set first_basic_block as entry into llvm_fn *)  
  llvm_fn  