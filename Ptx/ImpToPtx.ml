open Base 
open Imp
open Dict
open Ptx
open PtxType
open PtxHelpers
open PtxCodegen
open Printf 

let _ = Printexc.record_backtrace true

module Mem = MemspaceSemantics.Lattice

let same_type t1 t2 = 
  if t1 <> t2 then  debug $ Printf.sprintf "Expected same types, got %s and %s "
    (PtxType.to_str t1) (PtxType.to_str t2) 

let translate_coord = function 
  | Imp.X -> X
  | Imp.Y -> Y 
  | Imp.Z -> Z 
 
let translate_simple_exp codegen = function 
  | Imp.Var id -> codegen#get_data_reg id 
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

let gen_exp 
    tyMap  
    (memspaceMap : (ID.t, MemspaceSemantics.Lattice.t) PMap.t) 
    (codegen : PtxCodegen.ptx_codegen) 
    (destReg : PtxVal.value) 
    (exp : Imp.exp) = 
  let destType = PtxVal.type_of_var destReg in 
  debug $ Printf.sprintf "[imp2ptx->gen_exp] %s\n" (Imp.exp_to_str exp);
  let translate_arg exp' ptxT =
    let ptxVal = translate_simple_exp codegen exp' in 
    codegen#convert_fresh ~destType:ptxT ~srcVal:ptxVal
  in 
  match exp with 
  (* when dealing with a number or simple variable reference, just
     allocate a new register and move the value into it 
  *) 
  | e when is_simple_exp e -> 
      let t = PtxType.of_dyn_type (Imp.infer_dyn_type tyMap e) in
      same_type destType t;
      assert (destType = t);   
      let rhs = translate_simple_exp codegen e in
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
  
  | DimSize(dim, Var id) ->
      assert (PMap.mem id memspaceMap); 
      (match PMap.find id memspaceMap with 
        | Mem.SharedVec dims -> 
            assert (List.length dims <= dim); 
            let size = List.nth dims dim in 
            codegen#emit [mov destReg (PtxHelpers.int size)]
        | Mem.GlobalVec 1 ->
            let shapeReg = codegen#get_shape_reg id in 
            codegen#emit [ld_global U32 destReg shapeReg] 
            
        | _ -> failwith "dimsize not implemented for global vectors"
      )
  
  | Imp.Op(op,dynResultType, dynArgType, [x;y]) when Prim.is_binop op ->
      let ptxResultType = PtxType.of_dyn_type dynResultType in
      same_type ptxResultType destType;
      assert (ptxResultType = destType); 
      let ptxArgType = PtxType.of_dyn_type dynArgType in 
      let x' = translate_arg x ptxArgType in 
      let y' = translate_arg y ptxArgType in 
      let ptxop = PtxHelpers.prim_to_ptx_binop op ptxArgType in 
      codegen#emit [PtxHelpers.op3 ptxop destReg x' y'] 
 
  | Imp.Op (op,dynResultType, dynArgType, [x]) when Prim.is_unop op -> 
      let ptxResultType = PtxType.of_dyn_type dynResultType in
      same_type ptxResultType destType;
      assert (ptxResultType = destType); 
      let ptxArgType = PtxType.of_dyn_type dynArgType in
      let ptxop = PtxHelpers.prim_to_ptx_unop op ptxArgType in   
      let x' = translate_arg x ptxArgType in
      codegen#emit [PtxHelpers.op2 ptxop destReg x']
              
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
  | Imp.Idx(Var id, idx) -> 
      let baseReg = codegen#get_data_reg id in
      let idxDynT = Imp.infer_dyn_type tyMap idx in 
      let idxPtxT = PtxType.of_dyn_type idxDynT in 
      if (PtxType.nbytes idxPtxT <> 4) then 
        failwith (sprintf "array index expected to be 32-bit, \
                   received: %s" (PtxType.to_str idxPtxT));
      let idxReg = translate_arg idx idxPtxT in
      let memspace = PMap.find id memspaceMap in 
      let arrDynT = PMap.find id tyMap  in  
      let eltT = DynType.elt_type arrDynT in
      let gpuT = PtxType.of_dyn_type eltT in  
      let gpuStorageT = PtxType.storage_of_dyn_type eltT in 
      let eltBytes = PtxType.nbytes gpuStorageT in 
      begin match memspace with
      (* load a scalar from an array *) 
      | Mem.GlobalVec 1 
      | Mem.SharedVec [_] ->
          let regs = codegen#fresh_regs U64 3 in
          codegen#emit [ 
            comment "Calculating load index";
            cvt U64 U32 regs.(0) idxReg;            
            mul_lo U64 regs.(1) regs.(0) (int eltBytes);
            add U64 regs.(2) baseReg regs.(1);
          ]; 
          let storageReg = codegen#fresh_reg gpuStorageT in 
          if memspace =  Mem.GlobalVec 1 then 
            codegen#emit [ld_global gpuStorageT storageReg regs.(2)]
          else
            codegen#emit [ld_shared gpuStorageT storageReg regs.(2)]
          ;
          codegen#convert ~destReg  ~srcVal:storageReg 
          
      | _ -> failwith "Unexpected memory space for array indexing"
      end
  | Imp.Cast(tNew, tOld, x) ->
      let tNew' = PtxType.of_dyn_type tNew in 
      assert (tNew' = PtxVal.type_of_var destReg); 
      let tOld' = PtxType.of_dyn_type tOld in  
      let x' = translate_arg x tOld' in
      let oldName = DynType.to_str tOld in 
      let newName = DynType.to_str tNew in  
      codegen#emit [
        comment (sprintf "Conversion from %s to %s " oldName newName)
      ]; 
      codegen#convert ~destReg ~srcVal:x' 
      
             
  | other -> 
     failwith (sprintf "unexpected simple imp expression: %s" 
               (Imp.exp_to_str other))
  
  
let infer_ptx_type tenv exp = 
  PtxType.of_dyn_type (Imp.infer_dyn_type tenv exp)

  
(* given an array of n dimensions, return the reverse 
   cumulative shape vector needed to calculate indices 
*)  
let calc_index_widths dims eltSize =
  let n = Array.length dims in 
  let result = Array.copy dims in 
  result.(n-1) <- eltSize; 
  for i = n - 2 to 0 do 
    result.(i) <- dims.(i+1) * result.(i+1)
  done;
  result

                 
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

let compute_global_array_pos 
      codegen 
      rank 
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

         
let rec gen_stmt tyMap memspaceMap codegen stmt = 
  debug (Printf.sprintf "[imp2ptx->gen_stmt] %s" (Imp.stmt_to_str stmt)); 
  match stmt with 
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
      
  | Imp.Set (id,rhs) -> 
      
      let dynT = PMap.find id tyMap in
   
      let reg = codegen#declare_local id dynT in
      debug (sprintf "set imp var _x%d : %s => %d : %s" 
               id (DynType.to_str dynT)
               (PtxVal.get_id reg) (PtxType.to_str (PtxVal.type_of_var reg))
             );   
      gen_exp tyMap memspaceMap codegen reg rhs
      
  | Imp.SetIdx (id, indices, rhs) -> 
      let idxTypes = List.map (infer_ptx_type tyMap) indices in
      codegen#emit [comment "array store "];
      let idxRegs = List.map codegen#fresh_reg idxTypes in  
      List.iter2 
        (fun idx idxReg -> 
            gen_exp tyMap memspaceMap codegen idxReg idx)
         indices idxRegs; 
      (* TODO: should peel the vec as many times as we have indices *) 
      let rhsDynT = DynType.elt_type (PMap.find id tyMap) in  
      let rhsPtxT = PtxType.of_dyn_type rhsDynT in  
      let rhsReg = codegen#fresh_reg rhsPtxT in 
     
      gen_exp tyMap memspaceMap codegen rhsReg rhs; 
      let rhsStorageT = PtxType.storage_of_dyn_type rhsDynT in
      let rhsRegCvt = 
        codegen#convert_fresh ~destType:rhsStorageT ~srcVal:rhsReg 
      in  
      let base = codegen#get_data_reg id in
      codegen#emit [comment "calculating storage index"];
      let idxReg, idxT = match idxRegs, idxTypes with 
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
          codegen#emit [st_global rhsStorageT address rhsRegCvt]
        | Mem.SharedVec [_] -> 
          codegen#emit [st_shared rhsStorageT address rhsRegCvt] 
        | Mem.SharedVec _ -> 
          failwith "only 1D shared vectors implemented for array stores"
        | Mem.GlobalVec _ -> 
          failwith "only 1D global vectors implemented for array stores"
        | _ -> failwith "memory space not implemented yet for array stores"
      )
   

  | Imp.SyncThreads -> codegen#emit [PtxHelpers.bar]  
  | Imp.Comment str -> codegen#emit [PtxHelpers.comment str]
  | Imp.While (cond, block) -> 
      let predReg = codegen#fresh_reg Pred in 
      let testLabel = codegen#fresh_label in 
      codegen#emit [label testLabel (comment "loop test")]; 
      gen_exp tyMap memspaceMap codegen predReg cond;
      let exitLabel = codegen#fresh_label in 
      codegen#emit [pred_not predReg (bra exitLabel)]; 
      gen_block tyMap memspaceMap codegen block; 
      codegen#emit [
        bra testLabel;
        label exitLabel (comment "loop exit")
      ] 
      
  | Imp.If (cond, tBlock, fBlock) ->
      let predReg = codegen#fresh_reg Pred in
      gen_exp tyMap memspaceMap codegen predReg cond; 
      let endLabel = codegen#fresh_label in 
      let trueLabel = codegen#fresh_label in 
      codegen#emit [pred predReg (PtxHelpers.bra trueLabel)];
      gen_block tyMap memspaceMap codegen fBlock; 
      codegen#emit [bra endLabel; label trueLabel (comment "true branch")]; 
      gen_block tyMap memspaceMap codegen tBlock; 
      codegen#emit [label endLabel (comment "branches of if-stmt converge")]
      
  | Imp.SPLICE -> failwith "unexpected SPLICE stmt in Imp code"
 
and gen_block tyMap memspaceMap codegen block = 
  List.iter (gen_stmt tyMap memspaceMap codegen) block
   
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
  let memspaceMap = ImpInferMemspace.analyze_function impfn in
  let tyMap = impfn.tenv in 
  gen_block tyMap memspaceMap codegen impfn.body; 
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