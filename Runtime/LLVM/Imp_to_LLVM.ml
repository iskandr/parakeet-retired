let array_field_to_idx = function  
  | RangeStart -> 0
  | RangeStop -> 1
  | ShiftData -> 0
  | ShiftAmt -> 1
  | ShiftDim -> 2
  | ShiftDefault -> 3 
  | RotData -> 0
  | RotDim -> 1
  | RotAmt -> 2
  | SliceData -> 0
  | SliceDim -> 1
  | SliceStart -> 2
  | SliceStop -> 3
  | FrozenData -> 0
  | FrozenDim -> 1
  | FrozenIdx -> 2


let named_values:(string, llvalue) Hashtbl.t = Hashtble.create 10 in
  let rec compile_stmt_seq types body = 
    match body with
    | [] -> []
    | head :: tail -> compile_stmt types head :: llvm_fn types body
  and rec compile_stmt types imp_stmt = 
    match imp_stmt with
    | None -> ()
    | Some node ->
      begin
        try match node with
        | Imp.If (cond, t_b, f_b) -> assert false
        | Imp.While exp b -> assert false
        | Imp.Set id exp -> assert false
          let val_ = compile_expr types exp in
          let variable = try Hashtbl.find named_values name with
          | Not_found -> raise (Error "unknown variable name " ^ id)
          in
          ignore(build_store val_ variable builder);
          (* val_ *)
        | Imp.SetIdx id idxs exp -> assert false
        | Imp.SyncThreads -> assert false
        | Imp.Comment str -> assert false
        with Codegen.Error s ->
          print_endline s;
     end;
  and compile_expr types imp_expr = 
    match imp_expr with 
    | Imp.Val val_node -> compile_val types val_node
    | Imp.Op t op vals -> assert false
    | Imp.Select t cond t_v f_v -> assert false
    | Imp.Cast t value -> assert false
    | Imp.Idx var idxs -> assert false
    | Imp.DimSize var dim -> assert false
    | Imp.FreezeDim ? ? ? -> assert false
    | Imp.ArrayField ? ? -> assert false
  and compile_val types imp_val =
    match compile_val with
    | Imp.Var id -> 
      let v = try Hashtbl.find named_values name with
      | Not_found -> raise (Error "unkown variable name")
      in
      v
    | Imp.Const const -> Value_to_GenereicValue.to_llvm const
    | CudaInfo _ -> assert false


let compile_fn fn = 
  let inputTypes = List.map (fun id -> ID.Map.find id fn.types) fn.input_ids in
  let outputTypes = List.map (fun id -> ID.Map.find id fn.types) fn.output_ids in  
  let llvm_fn = init_compiled_fn inputTypes outputTypes in 
  let first_basic_block = compile_stmt_seq llvm_fn fn.types fn.body in
  (* TODO: set first_basic_block as entry into llvm_fn *)  
  llvm_fn 