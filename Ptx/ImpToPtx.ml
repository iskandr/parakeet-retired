open Base 
open Imp
open Dict
open Ptx
open PtxType
open PtxHelpers
open PtxCodegen
open Printf 

let _ = Printexc.record_backtrace true


let same_type t1 t2 = 
  if t1 <> t2 then  debug $ Printf.sprintf "Expected same types, got %s and %s "
    (PtxType.to_str t1) (PtxType.to_str t2) 

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
        let size = dims.(dim) in 
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
      (* indexing into a 1D array yields a scalar *)  
      if rank = 1 then 
        let regs = codegen#fresh_regs U64 3 in
        codegen#emit [ 
          cvt U64 U32 regs.(0) idxReg;            
          mul_lo U64 regs.(1) regs.(0) (int eltBytes);
          add U64 regs.(2) baseReg regs.(1);
        ]; 
        let storageReg = codegen#fresh_reg gpuStorageT in 
        (if isShared then
          codegen#emit [ld_shared gpuStorageT storageReg regs.(2)] 
        else
          codegen#emit [ld_global gpuStorageT storageReg regs.(2)]
        )
      (* generate an array slice *)      
      else failwith "lolcats?"
  | Imp.Idx(_, _) -> failwith "[imp2ptx] attempted to index into non-array"
  | Imp.Cast(tNew, x) ->
      let tNewPtx = PtxType.of_dyn_type tNew in 
      assert (tNewPtx = PtxVal.type_of_var destReg);
      let tOld = x.exp_type in  
      let tOldPtx = PtxType.of_dyn_type tOld in  
      let x' = translate_arg x tOldPtx in
      let oldName = DynType.to_str tOld in 
      let newName = DynType.to_str tNew in  
      codegen#emit [
        comment (sprintf "Conversion from %s to %s " oldName newName)
      ]; 
      codegen#convert ~destReg ~srcVal:x' 
      
  | other -> 
     failwith (sprintf "unexpected simple imp expression: %s" 
               (Imp.exp_to_str other))
  end
  ; 
  destReg 
  (* given an array of n dimensions, return the reverse 
   cumulative shape vector needed to calculate indices 
*)  
(*

 *)                
(*
let gen_array_slice tyMap memspaceMap codegen lhsId baseId baseReg idxReg idxPtxT arrDynT eltBytes = 
  let nestedT = DynType.peel_vec arrDynT in 
  match PMap.find baseId memspaceMap with 
  |  Mem.GlobalVec rank -> 
    codegen#emit [comment "Calculating index into global array"];
    let newVecReg = codegen#declare_local lhsId nestedT in
    let newShapeReg = codegen#get_shape_reg lhsId in   
    let oldShapeReg = codegen#get_shape_reg baseId in 
    codegen#emit [add U64 ~dest:newShapeReg ~src1:oldShapeReg ~src2:(int 4)]; 
    (*
      compute the slice offset by multiplying components of the shape
      vector. We could keep overwriting the same slicesize
      register but computing the incremental products in 
      different registers may leave open more opportunities 
      for optimization.   
    *)
    let slicesizes = codegen#fresh_regs U64 rank in
    codegen#emit [mov slicesizes.(0) (int eltBytes)];
    for i = 1 to rank - 1 do
      let dim = codegen#fresh_reg U64  in
      codegen#emit [
        ld_global U64 dim newShapeReg ~offset:(4 * i);
        mul_lo U64 slicesizes.(i) slicesizes.(i-1) dim
      ]
    done;
    let idxReg64 = codegen#fresh_reg U64 in 
    codegen#emit [cvt U64 idxPtxT idxReg64 idxReg]; 
    let offset = codegen#fresh_reg U64 in
    codegen#emit [
      mul_lo U64 offset slicesizes.(rank - 1) idxReg64;   
      add U64 newVecReg baseReg offset; 
    ]
         
  |  Mem.SharedVec (_::tailDims) -> 
     codegen#emit [comment "Calculating index into shared array"];
     let newVecReg = codegen#declare_shared_slice lhsId in
     let dimprod = List.fold_left ( * ) 1 tailDims in
     let slicesize = dimprod * eltBytes in
     let idxReg64 = codegen#fresh_reg U64 in 
     codegen#emit [cvt U64 idxPtxT idxReg64 idxReg]; 
     let offset = codegen#fresh_reg U64 in
     codegen#emit [
       mul_lo U64 offset (int slicesize) idxReg64;   
       add U64 newVecReg baseReg offset; 
     ]
  | _ -> failwith "Unexpected memory space for array slicing"
*)
(*
let compute_global_array_pos 
      codegen 
      eltBytes 
      baseReg 
      shapeReg 
      idxReg 
      idxPtxT = 
  codegen#emit [comment "Calculating position in global array"];
  let slicesizes = codegen#fresh_regs U64 rank in
  codegen#emit [mov slicesizes.(0) (int eltBytes)];
  for i = 1 to rank - 1 do
    let dim = codegen#fresh_reg U64  in
    codegen#emit [
      ld_global U64 dim shapeReg ~offset:(4 * i);
      mul_lo U64 slicesizes.(i) slicesizes.(i-1) dim
    ]
  done;
  let idxReg64 = codegen#fresh_reg U64 in 
  codegen#emit [cvt U64 idxPtxT idxReg64 idxReg]; 
  let offset = codegen#fresh_reg U64 in
  let position = codegen#fresh_reg U64 in 
  codegen#emit [
      mul_lo U64 offset slicesizes.(rank - 1) idxReg64;   
      add U64 position baseReg offset; 
  ];
  position  
*)
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
    let multReg = codegen#fresh_reg U64 in
    for i = 0 to numIndices - 1 do 
      codegen#emit [
        PtxHelpers.mul_lo U64 multReg idxRegs.(i) (PtxHelpers.int widths.(i));
        PtxHelpers.add U64 address address multReg
      ]
    done
  else
    let shapeReg = codegen#get_shape_reg baseReg in
    let shapeEltReg = codegen#fresh_reg U32 in  
    let shapeEltReg64 = codegen#fresh_reg U64 in
    let multReg = codegen#fresh_reg U64 in 
    for i = 0 to numIndices do
      codegen#emit [ld_global U32 shapeEltReg shapeReg ~offset:(4*i)]; 
      codegen#convert ~destReg:shapeEltReg64 ~srcVal:shapeEltReg; 
      codegen#emit [
        PtxHelpers.mul_lo U64 multReg idxRegs.(i) shapeEltReg64;
        PtxHelpers.add U64 address address multReg
      ]
    done 
 end; address 
                    
        
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
   
let translate_kernel impfn = 
  let codegen = new ptx_codegen in
  (* declare all local variables as either a scalar register or 
     a shared array. 
  *)
  Array.iter2 
    (fun id dynT -> 
      if PMap.mem id impfn.shared then
        let dims = PMap.find id impfn.shared in   
        codegen#declare_shared_vec id (DynType.elt_type dynT) dims
      else 
        codegen#declare_local id dynT
    ) 
    impfn.local_ids impfn.local_types 
  ;
  Array.iter2 codegen#declare_arg impfn.input_ids impfn.input_types;
  Array.iter2 codegen#declare_arg impfn.output_ids impfn.output_types; 
  gen_block codegen impfn.body; 
  codegen#run_rewrite_pass PtxSimplify.simplify;   
  codegen#finalize_kernel


(*
  (* fused multiply and add *) 
  | Imp.Op(Prim.Add,addT, [Imp.Op(Prim.Mult,mulT, [x; y]); z])
  | Imp.Op(Prim.Add,addT, [z; Imp.Op(Prim.Mult,mulT, [x; y])])   
      when addT = mulT -> 
      codegen#emit [comment "optimizing mad"];
      let t = PtxType.of_dyn_type addT in 
      let x' = translate_arg x t in 
      let y' = translate_arg y t in 
      let z' = translate_arg z t in 
      codegen#emit [PtxHelpers.mad t destReg x' y' z']
  *)
  
  
  (*
  
  (* treat slicing as a special case. Relies on Imp code having been flattened
     so that slices can only at the outermost level of a stmt. 
  *)
  | Imp.Set (lhsId, Idx(Var baseId, idx))  ->
      let idxT = Imp.infer_dyn_type tyMap idx in 
      let idxPtxT = PtxType.of_dyn_type idxT in 
      if (PtxType.nbytes idxPtxT <> 4) then 
        failwith "array index expected to be 32-bit";
      let idxReg = codegen#fresh_reg idxPtxT in 
      gen_exp tyMap memspaceMap codegen idxReg idx; 
      let arrDynT = PMap.find baseId tyMap  in
      let nestDepth = DynType.nest_depth arrDynT in
      let baseReg = translate_simple_exp codegen (Var baseId) in
      let eltT = DynType.elt_type arrDynT in
      let eltPtxT = PtxType.of_dyn_type eltT in
      let storageT =  PtxType.storage_of_dyn_type eltT  in 
      let eltBytes = PtxType.nbytes storageT in     
      if nestDepth > 1 then (
        codegen#emit [comment "generating array slice"];
        gen_array_slice 
          tyMap 
          memspaceMap 
          codegen 
          lhsId
          baseId 
          baseReg 
          idxReg 
          idxPtxT
          arrDynT 
          eltBytes
      ) else if nestDepth = 1 then (
  
        codegen#emit [comment "generating array load"];
        let resultReg = codegen#declare_local lhsId eltT in
        let storageReg = codegen#fresh_reg storageT in 
        (match PMap.find baseId memspaceMap with 
          | Mem.GlobalVec rank ->
            let shapeReg = codegen#get_shape_reg baseId in
            let posReg = compute_global_array_pos 
              codegen 1 eltBytes baseReg shapeReg idxReg idxPtxT in
            codegen#emit [ld_global storageT storageReg posReg];
          | Mem.SharedVec [_] ->
              let position = codegen#fresh_reg U64 in
              let offset = codegen#fresh_reg U64 in  
              let idxReg64 = codegen#convert_fresh U64 idxReg in 
              codegen#emit [
                mul_lo U64 offset idxReg64 (PtxHelpers.int eltBytes);   
                add U64 position baseReg offset; 
                ld_shared storageT storageReg position
              ]
          | _ -> 
             failwith "expected 1D memory space descriptor in imp->ptx store"
        );
        assert (eltPtxT = PtxVal.type_of_var resultReg);  
        codegen#convert ~destReg:resultReg ~srcVal:storageReg 
     )      
  *)
  (*   let idxReg, idxT = match idxRegs, idxTypes with 
        | [idxReg], [idxT] -> idxReg, idxT
        | _ -> failwith "ptx->imp only supports 1 dimensional array stores"
      in 
      let idxReg' = 
        codegen#convert_fresh ~destType:PtxType.U64 ~srcVal:idxReg 
      in
        (* get offset as product of index and storage bytesize *)   
      let offset = codegen#fresh_reg PtxType.U64 in
      (* compute linear offset into data array as sum of base and offset *) 
      let address = codegen#fresh_reg U64 in  
      codegen#emit [
          mul_lo PtxType.U64 offset idxReg' (int $ PtxType.nbytes rhsStorageT);
          add U64 address base offset    
      ];
      (* for now only works with 1d arrays *)  
      (match PMap.find id memspaceMap with 
        | Mem.GlobalVec 1 ->
         
        | Mem.SharedVec [_] -> 
           
        | Mem.SharedVec _ -> 
          failwith "only 1D shared vectors implemented for array stores"
        | Mem.GlobalVec _ -> 
          failwith "only 1D global vectors implemented for array stores"
        | _ -> failwith "memory space not implemented yet for array stores"
      )
      *)