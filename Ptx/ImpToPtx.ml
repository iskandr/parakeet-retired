(* pp: -parser o pa_macro.cmo *)

open Base 
open Imp
open Ptx
open PtxType
open PtxCodegen
open Printf 

let same_type t1 t2 = 
  if t1 <> t2 then  debug $ Printf.sprintf "Expected same types, got %s and %s "
    (PtxType.to_str t1) (PtxType.to_str t2) 

let translate_coord = function | Imp.X -> X | Imp.Y -> Y  | Imp.Z -> Z 
 
let translate_simple_exp (codegen:PtxCodegen.ptx_codegen) = function 
  | Imp.Var id -> codegen#imp_reg id 
  | Imp.Const n -> num_to_ptx_const n
  | Imp.GridDim coord  -> 
     codegen#cached $ Special (NumCtaId (translate_coord coord))
  | Imp.BlockDim coord ->
     codegen#cached $ Special (NumThreadId (translate_coord coord))
  | Imp.BlockIdx coord  -> 
     codegen#cached $ Special (CtaId (translate_coord coord))
  | Imp.ThreadIdx coord ->
     codegen#cached $ Special (ThreadId (translate_coord coord))
  | other -> failwith $ 
           Printf.sprintf "cannot translate complex Imp expression: %s\n"
           (Imp.exp_to_str other)  
            
(* translate a simple expression and potentially convert it to some desired
   type 
*) 
let gen_exp 
    (codegen : PtxCodegen.ptx_codegen)
    ?destReg
    (expNode : Imp.exp_node) : PtxVal.value  =
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
  | Imp.Op(op,dynArgType, args) -> 
      same_type ptxResultT destType;
      assert (ptxResultT = destType);
      let ptxArgT = PtxType.of_dyn_type dynArgType in
      let args' = List.map (fun x -> translate_arg x ptxArgT) args in
      let ptxop = prim_to_ptx_op op ptxArgT in 
      codegen#emit [mkop ptxop (destReg::args')]
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
      let eltBytes = 
        PtxType.nbytes 
          (PtxType.storage_of_dyn_type (DynType.elt_type dynResultT))
      in
      let isShared = codegen#is_shared_ptr baseReg in 
      if not isShared && not (codegen#is_global_array_ptr baseReg) then 
        failwith "[imp2ptx] cannot generate indexing code: \"
                 base address register is neither global nor shared pointer"
      else 
      let rank = codegen#get_array_rank baseReg in
      let address = codegen#compute_address baseReg eltBytes [|idxReg|] in  
      (* indexing into a 1D array yields a scalar *)  
      if rank = 1 then (
        let gpuStorageT = PtxType.storage_of_dyn_type dynResultT in 
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
  
  | DimSize(dim, {Imp.exp=Var id}) ->
      let arrayReg = codegen#imp_reg id in 
      if codegen#is_shared_ptr arrayReg then 
        let dims = codegen#get_shared_dims arrayReg in 
        assert (dim <= Array.length dims);
        let size = dims.(dim - 1) in 
        codegen#emit [mov destReg (int size)]
     else if codegen#is_global_array_ptr arrayReg then 
        let rank = codegen#get_global_array_rank arrayReg in 
        assert (dim <= rank);
        let shapeReg = codegen#get_shape_reg arrayReg in
        (* this works if we're looking at 1st dimension, 
           what about bigger ones?..we need constant offset + dim-1*32 
        *) 
        codegen#emit [ld_global U32 destReg shapeReg] 
     else failwith "[imp2ptx] attempting to get DimSize of a scalar"
  
  (* when dealing with a constant or simple variable reference, just
     allocate a new register and move the value into it. 
     Note that although Imp array dimensions are also technically constants, 
     we deal with them separately above. 
  *) 
 
  | ThreadIdx _ 
  | BlockIdx _ 
  | BlockDim _ 
  | GridDim _    
  | Const _ 
  | Var _ ->
      same_type destType ptxResultT;
      assert (destType = ptxResultT);
      let rhs = translate_simple_exp codegen exp in
      codegen#emit [mov destReg rhs]      
  | other -> 
     failwith (sprintf "unexpected imp expression: %s" 
               (Imp.exp_to_str other))
  end
  ; 
  destReg 

let rec gen_stmt codegen stmt = 
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
       let address = codegen#compute_address base eltSize idxRegs in 
       let rhsReg = gen_exp codegen rhs in 
       let rhsRegCvt = 
         codegen#convert_fresh ~destType:rhsStorageT ~srcVal:rhsReg 
       in
       (if codegen#is_shared_ptr base then 
          codegen#emit [st_shared rhsStorageT address rhsRegCvt]
        else
          codegen#emit [st_global rhsStorageT address rhsRegCvt]
       ) 
  | Imp.SyncThreads -> codegen#emit [bar]  
  | Imp.Comment str -> codegen#emit [comment ("Imp comment: " ^ str)]
  | Imp.While (cond, block) ->
      let testLabel = codegen#fresh_label in
      codegen#emit [label testLabel (comment "loop test")];
      let predReg = gen_exp codegen cond in
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
      codegen#emit [pred predReg (bra trueLabel)];
      gen_block codegen fBlock; 
      codegen#emit [bra endLabel; label trueLabel (comment "true branch")]; 
      gen_block codegen tBlock; 
      codegen#emit [label endLabel (comment "branches of if-stmt converge")]
  | Imp.SPLICE -> failwith "unexpected SPLICE stmt in Imp code"
 
and gen_block codegen block = 
  List.iter (gen_stmt codegen) block
   
let translate_kernel ?input_spaces (impfn : Imp.fn) =
  IFDEF DEBUG THEN 
    print_string  "\n[imp2ptx] ***** started translation ****\n";
    print_string (Imp.fn_to_str impfn);
    print_string "\n";
    print_string "\n[imp2ptx] ******************************\n";
  ENDIF;
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
  let codegen = new PtxCodegen.ptx_codegen in
  (* declare all local variables as:
      1) a scalar register 
      2) a local array for which you receive a pointer 
      3) a shared array 
  *)
  let register_local id dynT = 
    (* todo: this is wrong since we've change array annotations *)
    if PMap.mem id impfn.shared_array_allocations then
      let dims = PMap.find id impfn.shared_array_allocations in   
        ignore $ codegen#declare_shared_vec id (DynType.elt_type dynT) dims
      else (
        if PMap.mem id impfn.local_arrays then ( 
          IFDEF DEBUG THEN 
            Printf.printf "[imp2ptx] declaring local array %s : %s\n" 
              (ID.to_str id)
              (DynType.to_str dynT);
          ENDIF;   
          ignore $ codegen#declare_storage_arg id dynT
         ) 
         else
           ignore $ codegen#declare_local id dynT
      )
  in 
  Array.iter2 register_local impfn.local_ids impfn.local_types;
  Array.iter3 
    codegen#declare_input impfn.input_ids impfn.input_types inputSpaces;
  Array.iter2 codegen#declare_output impfn.output_ids impfn.output_types; 
  gen_block codegen impfn.body; 
  codegen#finalize_kernel 