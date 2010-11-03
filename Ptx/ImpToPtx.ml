open Base 
open Imp
open Ptx
open PtxType
open PtxHelpers
open PtxCodegen
open Printf 

let _ = Printexc.record_backtrace true


let same_type t1 t2 = 
  if t1 <> t2 then  debug $ Printf.sprintf "Expected same types, got %s and %s "
    (PtxType.to_str t1) (PtxType.to_str t2) 


let calc_index_widths dims eltSize =
  let n = Array.length dims in 
  let result = Array.copy dims in 
  result.(n-1) <- eltSize; 
  for i = n - 2 downto 0 do 
    result.(i) <- dims.(i+1) * result.(i+1)
  done;
  result

let compute_address codegen baseReg eltSize (idxRegs : PtxVal.value array) = 
  let rank = codegen#get_array_rank baseReg in
  let numIndices = Array.length idxRegs in  
  let baseReg64 = codegen#convert_fresh ~destType:U64 ~srcVal:baseReg in
  let idxRegs64 = 
    Array.map 
      (fun reg -> codegen#convert_fresh ~destType:U64 ~srcVal:reg) 
      idxRegs 
  in
  let address = codegen#fresh_reg U64 in
  codegen#emit [mov address baseReg64]; 
  begin 
  if codegen#is_shared_ptr baseReg then 
    let widths = calc_index_widths (codegen#get_shared_dims baseReg) eltSize in
    for i = 0 to numIndices - 1 do
      let multReg = codegen#fresh_reg U64 in 
      codegen#emit [
        PtxHelpers.mul_lo U64 multReg idxRegs64.(i) (PtxHelpers.int widths.(i));
        PtxHelpers.add U64 address address multReg
      ]
    done
  else
    let shapeReg = codegen#get_shape_reg baseReg in
    for i = 0 to numIndices - 1 do
      (* size of slice through array each index accounts for *) 
      let sliceReg = codegen#fresh_reg U64 in
      (* at the very least each increase in this index has to go up by the
         bytesize of the element
      *) 
      codegen#emit [mov sliceReg (int eltSize)];
      (* indexing the first dimension effectively slices through all 
         other dimensions. If this is, however, a 1D vector then this 
         loop never runs
      *) 
      for j = (i+1) to rank - 1 do 
        let shapeEltReg = codegen#fresh_reg U32 in
        let shapeEltReg64 = codegen#fresh_reg U64 in
        codegen#emit [ld_global U32 shapeEltReg shapeReg ~offset:(4*j)];
        codegen#convert ~destReg:shapeEltReg64 ~srcVal:shapeEltReg; 
        codegen#emit [mul_lo U64 sliceReg sliceReg shapeEltReg64]  
      done;  
      let offset = codegen#fresh_reg U64 in 
      codegen#emit [
        mul U64 offset idxRegs64.(i) sliceReg;
        add U64 address address offset  
      ]
    done 
 end; address 

let translate_coord = function 
  | Imp.X -> X
  | Imp.Y -> Y 
  | Imp.Z -> Z 
 
let translate_simple_exp codegen = function 
  | Imp.Var id -> codegen#imp_reg id 
  | Imp.Const n -> PtxHelpers.num_to_ptx_const n 
  | other -> failwith $ 
           Printf.sprintf "cannot translate complex Imp expression: %s\n"
           (Imp.exp_to_str other)  
          
(* translate a simple expression and potentially convert it to some desired
   type 
*) 

let translate_special = function 
  | Imp.GridDim coord ->  Special (NumCtaId (translate_coord coord))
  | Imp.BlockDim coord ->  Special (NumThreadId (translate_coord coord))
  | Imp.BlockIdx coord  -> Special (CtaId (translate_coord coord))
  | Imp.ThreadIdx coord -> Special (ThreadId (translate_coord coord))
  | _ -> failwith "[imp2ptx] expected special register"

let gen_exp 
    (codegen : PtxCodegen.ptx_codegen)
    ?destReg
    (expNode : Imp.exp_node) : PtxVal.value  =
  (* debug $ Printf.sprintf "[imp2ptx] --- exp: %s" (Imp.exp_node_to_str expNode); *)
  let dynResultT = expNode.Imp.exp_type in 
  let ptxResultT = PtxType.of_dyn_type dynResultT in  
  let destReg, destType = match destReg with 
    | Some destReg -> destReg, PtxVal.type_of_var destReg
    | None -> codegen#fresh_reg ptxResultT, ptxResultT 
  in   
  let translate_arg expNode' ptxT =
    let ptxVal = translate_simple_exp codegen expNode'.exp in 
    codegen#convert_fresh ~destType:ptxT ~srcVal:ptxVal
  in 
  let exp = expNode.Imp.exp in 
  begin match exp with 
  (* when dealing with a number or simple variable reference, just
     allocate a new register and move the value into it 
  *) 
  | Imp.Var _  | Imp.Const _ ->
      same_type destType ptxResultT;
      assert (destType = ptxResultT);  
      let rhs = translate_simple_exp codegen exp in
      codegen#emit [mov destReg rhs]  
  | Imp.GridDim _ 
  | Imp.BlockDim _ 
  | Imp.BlockIdx _  
  | Imp.ThreadIdx _ -> 
      let special = translate_special exp in  
      if destType = U16 then 
        codegen#emit [mov destReg special]
      else
        let reg16 = codegen#fresh_reg U16 in  begin
          codegen#emit [mov reg16 special]; 
          codegen#convert ~destReg:destReg ~srcVal:reg16
        end
  
  | DimSize(dim, {Imp.exp=Var id}) ->
      let arrayReg = codegen#imp_reg id in 
      if codegen#is_shared_ptr arrayReg then 
        let dims = codegen#get_shared_dims arrayReg in 
        assert (dim <= Array.length dims);
        let size = dims.(dim - 1) in 
        codegen#emit [mov destReg (PtxHelpers.int size)]
     else if codegen#is_global_array_ptr arrayReg then 
        let rank = codegen#get_global_array_rank arrayReg in 
        assert (dim <= rank);
        let shapeReg = codegen#get_shape_reg arrayReg in
        (* this works if we're looking at 1st dimension, 
           what about bigger ones?..we need constant offset + dim-1*32 
        *) 
        codegen#emit [ld_global U32 destReg shapeReg] 
     else failwith "[imp2ptx] attempting to get DimSize of a scalar"       
  | Imp.Op(op,dynArgType, args) -> 
      same_type ptxResultT destType;
      assert (ptxResultT = destType);
      let ptxArgT = PtxType.of_dyn_type dynArgType in
      let args' = List.map (fun x -> translate_arg x ptxArgT) args in
      let ptxop = PtxHelpers.prim_to_ptx_op op ptxArgT in 
      codegen#emit [PtxHelpers.mkop ptxop (destReg::args')]
   | Imp.Select(t, cond, trueExp, falseExp) -> 
      let t' = PtxType.of_dyn_type t in
      assert (t' = destType);  
      let cond' = translate_arg cond Pred in
      let trueExp' = translate_arg trueExp t' in 
      let falseExp' = translate_arg falseExp t' in
      codegen#emit [
        selp ~dest:destReg ~ifTrue:trueExp' ~ifFalse:falseExp' ~cond:cond' 
      ]
  (* by this point all the index expressions should have been flattened, 
     so only expect a variable on the lhs 
  *) 
  | Imp.Idx({exp=Var id; exp_type=arrDynT}, idx) -> 
      let baseReg = codegen#imp_reg id in
      let idxDynT = idx.exp_type in 
      let idxPtxT = PtxType.of_dyn_type idxDynT in 
      if (PtxType.nbytes idxPtxT <> 4) then 
        failwith $ 
          sprintf "[imp2ptx] array index expected to be 32-bit, received: %s" 
          (PtxType.to_str idxPtxT);
      let idxReg = translate_arg idx idxPtxT in
      let gpuEltT = PtxType.of_dyn_type dynResultT in  
      let gpuStorageT = PtxType.storage_of_dyn_type dynResultT in 
      let eltBytes = PtxType.nbytes gpuStorageT in
      let isShared = codegen#is_shared_ptr baseReg in 
      if not isShared && not (codegen#is_global_array_ptr baseReg) then 
        failwith "[imp2ptx] cannot generate indexing code: \"
                 base address register is neither global nor shared pointer"
      else 
      let rank = codegen#get_array_rank baseReg in
      let address = compute_address codegen baseReg eltBytes [|idxReg|] in  
      (* indexing into a 1D array yields a scalar *)  
      if rank = 1 then (
        let storageReg = codegen#fresh_reg gpuStorageT in 
        if isShared then
          codegen#emit [ld_shared gpuStorageT storageReg address] 
        else
          codegen#emit [ld_global gpuStorageT storageReg address]
        ;
        codegen#convert ~destReg ~srcVal:storageReg
      )
      (* generate an array slice *)      
      else (
        codegen#convert ~destReg ~srcVal:address; 
        codegen#declare_slice baseReg destReg 
     )     
           
  | Imp.Idx(_, _) -> failwith "[imp2ptx] attempted to index into non-array"
  | Imp.Cast(tNew, x) ->
      let tNewPtx = PtxType.of_dyn_type tNew in 
      assert (tNewPtx = PtxVal.type_of_var destReg);
      let tOld = x.exp_type in  
      let tOldPtx = PtxType.of_dyn_type tOld in  
      let x' = translate_arg x tOldPtx in
      let oldName = DynType.to_str tOld in 
      let newName = DynType.to_str tNew in  
      codegen#convert ~destReg ~srcVal:x' 
      
  | other -> 
     failwith (sprintf "unexpected simple imp expression: %s" 
               (Imp.exp_to_str other))
  end
  ; 
  destReg 

let rec gen_stmt codegen stmt = 
  (*debug $ Printf.sprintf "[imp2ptx] stmt: %s" (Imp.stmt_to_str stmt);*) 
  codegen#emit [comment (Imp.stmt_to_str stmt)];
  match stmt with 
  | Imp.Set (id,rhs) ->
      let dynT = rhs.exp_type in
      let reg = codegen#imp_reg id in   
      ignore (gen_exp codegen ~destReg:reg rhs)
      
  | Imp.SetIdx (id, indices, rhs) ->
      let idxRegs = Array.map (gen_exp codegen) (Array.of_list indices) in
      let base = codegen#imp_reg id in
      let rank = codegen#get_array_rank base in
      let numIndices = Array.length idxRegs in   
      if rank > numIndices  then
        failwith "[imp2ptx] Cannot set an array slice"
      else if rank < numIndices then 
        failwith "[imp2ptx] Too many indices given to setidx"
      else
       let rhsT = rhs.exp_type in 
       let rhsEltT = DynType.elt_type rhsT in
       let rhsStorageT = PtxType.storage_of_dyn_type rhsT in
       let eltSize = PtxType.nbytes rhsStorageT in   
       let address = compute_address codegen base eltSize idxRegs in 
       let rhsReg = gen_exp codegen rhs in 
       let rhsRegCvt = 
         codegen#convert_fresh ~destType:rhsStorageT ~srcVal:rhsReg 
       in
       (if codegen#is_shared_ptr base then 
          codegen#emit [st_shared rhsStorageT address rhsRegCvt]
        else
          codegen#emit [st_global rhsStorageT address rhsRegCvt]
       ) 
  | Imp.SyncThreads -> codegen#emit [PtxHelpers.bar]  
  | Imp.Comment str -> codegen#emit [PtxHelpers.comment ("Imp comment: " ^ str)]
  | Imp.While (cond, block) -> 
      let predReg = gen_exp codegen  cond in 
      let testLabel = codegen#fresh_label in 
      codegen#emit [label testLabel (comment "loop test")]; 
      let exitLabel = codegen#fresh_label in 
      codegen#emit [pred_not predReg (bra exitLabel)]; 
      gen_block codegen block; 
      codegen#emit [
        bra testLabel;
        label exitLabel (comment "loop exit")
      ] 
  | Imp.If (cond, tBlock, fBlock) ->
      let predReg = gen_exp codegen cond in  
      let endLabel = codegen#fresh_label in 
      let trueLabel = codegen#fresh_label in 
      codegen#emit [pred predReg (PtxHelpers.bra trueLabel)];
      gen_block codegen fBlock; 
      codegen#emit [bra endLabel; label trueLabel (comment "true branch")]; 
      gen_block codegen tBlock; 
      codegen#emit [label endLabel (comment "branches of if-stmt converge")]
  | Imp.SPLICE -> failwith "unexpected SPLICE stmt in Imp code"
 
and gen_block codegen block = 
  List.iter (gen_stmt codegen) block
   
let translate_kernel 
    (impfn : Imp.fn) 
    ?input_spaces 
    (allocSet : ID.Set.t) =

  debug "[imp2ptx] ***** started translation ****";
  debug $ Imp.fn_to_str impfn;
  debug "[imp2ptx] ******************************";
  
  let inputSpaces = match input_spaces with
    (* if we have no preferences about the space our inputs live in, 
       put all scalars in PARAM and all vectors in GLOBAL 
    *) 
    | None -> 
       Array.map
         (fun t -> if DynType.is_scalar t then PtxVal.PARAM else PtxVal.GLOBAL) 
         impfn.input_types
    | Some inputSpaces -> inputSpaces  
  in 
  let codegen = new ptx_codegen in
  (* declare all local variables as:
      1) a scalar register 
      2) a local array for which you receive a pointer 
      3) a shared array 
  *)
  let register_local id dynT = 
    if PMap.mem id impfn.shared then
      let dims = PMap.find id impfn.shared in   
        ignore $ codegen#declare_shared_vec id (DynType.elt_type dynT) dims
      else begin 
        if ID.Set.mem id allocSet then 
          ignore $ codegen#declare_storage_arg id dynT 
         else
           ignore $ codegen#declare_local id dynT
      end 
  in 
  Array.iter2 register_local impfn.local_ids impfn.local_types;
  let register_input id dynT space = 
    if space = PtxVal.GLOBAL || space = PtxVal.PARAM then 
      ignore $ codegen#declare_input id dynT 
    else if space = PtxVal.TEX then 
      ignore $ codegen#declare_texture_input id dynT  
    else failwith "unexpected input space"
  in   
  Array.iter3 register_input impfn.input_ids impfn.input_types inputSpaces;
  Array.iter2 codegen#declare_output impfn.output_ids impfn.output_types; 
  gen_block codegen impfn.body; 
  codegen#finalize_kernel 
  

