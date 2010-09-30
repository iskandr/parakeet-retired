open Base 
open Imp
open Dict
open Ptx
open PtxType
open PtxHelpers
open PtxCodegen
open Printf 


let translate_coord = function 
  | Imp.X -> X
  | Imp.Y -> Y 
  | Imp.Z -> Z 
 
let translate_simple_exp codegen = function 
  | Imp.Var id -> codegen#get_data_reg id 
  | Imp.Const n -> PtxHelpers.num_to_ptx_const n 
  | Imp.ThreadIdx coord -> 
      let reg16 = codegen#fresh_reg U16 in 
      let special = Special (ThreadId (translate_coord coord)) in 
      codegen#emit [mov U16 reg16 special]; 
      codegen#convert ~destType:U32  ~srcType:U16   ~srcReg:reg16
      
  | Imp.BlockIdx coord -> 
      let reg16 = codegen#fresh_reg U16 in
      let special = Special (CtaId (translate_coord coord)) in  
      codegen#emit [mov U16 reg16 special]; 
      codegen#convert ~destType:U32 ~srcType:U16  ~srcReg:reg16
  
  | Imp.BlockDim coord -> 
      let reg16 = codegen#fresh_reg U16 in
      let special = Special (NumThreadId (translate_coord coord)) in 
      codegen#emit [mov U16 reg16 special]; 
      codegen#convert ~destType:U32 ~srcType:U16 ~srcReg:reg16
  | Imp.GridDim coord -> 
      let reg16 = codegen#fresh_reg U16 in
      let special = Special (NumCtaId (translate_coord coord)) in 
      codegen#emit [mov U16 reg16 special];
      codegen#convert ~destType:U32 ~srcType:U16  ~srcReg:reg16 
      
  | _ -> failwith "cannot translate compound Imp expression" 


let gen_exp tyMap memspaceMap codegen  destReg exp = 
  let rec translate_arg e t = 
    if is_simple_exp e then translate_simple_exp codegen  e 
    else let reg = codegen#fresh_reg t in (aux reg e; reg)

  and aux destReg  = function
  | e when is_simple_exp e -> 
      let t = PtxType.of_dyn_type (Imp.infer_dyn_type tyMap e) in 
      let rhs = translate_simple_exp codegen e in
      codegen#emit [PtxHelpers.mov t destReg rhs]  
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
            
  | Imp.Op(op,t,[x;y]) when Prim.is_binop op -> 
      let t' = PtxType.of_dyn_type t in 
      let x' = translate_arg x t' in 
      let y' = translate_arg y t' in 
      let ptxop = PtxHelpers.prim_to_ptx_binop op t' in 
      codegen#emit [PtxHelpers.op3 ptxop destReg x' y'] 
 
  | Imp.Op (op,t,[x]) when Prim.is_unop op -> 
      let t' = PtxType.of_dyn_type t in
      let ptxop = PtxHelpers.prim_to_ptx_unop op t' in   
      let x' = translate_arg x t' in
      codegen#emit [PtxHelpers.op2 ptxop destReg x']        
  | Imp.Select(t, cond, trueExp, falseExp) -> 
      let t' = PtxType.of_dyn_type t in 
      let cond' = translate_arg cond Pred in
      let trueExp' = translate_arg trueExp t' in 
      let falseExp' = translate_arg falseExp t' in
      codegen#emit [
        PtxHelpers.selp t' 
          ~dest:destReg 
          ~ifTrue:trueExp' 
          ~ifFalse:falseExp' 
          ~cond:cond' 
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
      | ImpInferMemspace.GlobalVec 1 
      | ImpInferMemspace.SharedVec [_] ->
          let regs = codegen#fresh_regs U64 3 in
          codegen#emit [ 
            comment "Calculating load index";
            cvt U64 U32 regs.(0) idxReg;            
            mul_lo U64 regs.(1) regs.(0) (int eltBytes);
            add U64 regs.(2) baseReg regs.(1);
          ]; 
          let storageReg = codegen#fresh_reg gpuStorageT in 
          if memspace = ImpInferMemspace.GlobalVec 1 then 
            codegen#emit [ld_global gpuStorageT storageReg regs.(2)]
          else
            codegen#emit [ld_shared gpuStorageT storageReg regs.(2)]
          ;
          let cvtReg = 
            codegen#convert 
              ~destType:gpuT  
              ~srcType:gpuStorageT 
              ~srcReg:storageReg 
          in
          codegen#emit [PtxHelpers.mov gpuT destReg cvtReg]
      | _ -> failwith "Unexpected memory space for array indexing"
      end
  | Imp.Cast(tNew, tOld, x) ->
      let tNew' = PtxType.of_dyn_type tNew in 
      let tOld' = PtxType.of_dyn_type tOld in  
      let x' = translate_arg x tOld' in
      let oldName = DynType.to_str tOld in 
      let newName = DynType.to_str tNew in  
      codegen#emit [
        comment (sprintf "Conversion from %s to %s " oldName newName)
      ]; 
      let cvtReg = codegen#convert  ~destType:tNew' ~srcType:tOld' ~srcReg:x' in 
      codegen#emit [PtxHelpers.mov tNew' destReg cvtReg]
  | other -> 
     failwith (sprintf "unexpected simple imp expression: %s" 
               (Imp.exp_to_str other))
  
  in aux destReg exp 
  
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
  | ImpInferMemspace.GlobalVec rank -> 
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
    codegen#emit [mov U64 slicesizes.(0) (int eltBytes)];
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
         
  | ImpInferMemspace.SharedVec (_::tailDims) -> 
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

let compute_global_array_pos codegen rank eltBytes baseReg shapeReg idxReg idxPtxT = 
  codegen#emit [comment "Calculating position in global array"];
  let slicesizes = codegen#fresh_regs U64 rank in
  codegen#emit [mov U64 slicesizes.(0) (int eltBytes)];
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
  match stmt with 
  (* treat slicing as a special case. Relies on Imp code having been flattened
     so that slices can only at the outermost level of a stmt. 
  *)
  | Imp.Set (lhsId, Idx(Var baseId, idx))  ->
      let idxT = Imp.infer_dyn_type tyMap idx in 
      let idxPtxT = PtxType.of_dyn_type idxT in 
      if (PtxType.nbytes idxPtxT <> 4) then 
        failwith "array index expected to be 32-bit";
      let idxReg = translate_simple_exp codegen idx in 
      let arrDynT = PMap.find baseId tyMap  in
      let nestDepth = DynType.nest_depth arrDynT in
      let baseReg = translate_simple_exp codegen (Var baseId) in
      let eltT = DynType.elt_type arrDynT in
      let eltPtxT = PtxType.of_dyn_type eltT in
      let storageT =  PtxType.storage_of_dyn_type eltT  in 
      let eltBytes = PtxType.nbytes storageT in     
      if nestDepth > 1 then (
        codegen#emit [comment "generating array slice"];
        gen_array_slice tyMap memspaceMap codegen lhsId baseId baseReg idxReg idxPtxT arrDynT eltBytes
      ) else if nestDepth = 1 then (
  
        codegen#emit [comment "generating array load"];
        let resultReg = codegen#declare_local lhsId eltT in
        match PMap.find baseId memspaceMap with 
        | ImpInferMemspace.GlobalVec rank ->
 
          let shapeReg = codegen#get_shape_reg baseId in 
          let posReg = compute_global_array_pos 
             codegen 1 eltBytes baseReg shapeReg idxReg idxPtxT in
          let storageReg = codegen#fresh_reg storageT in 
          codegen#emit [ld_global storageT storageReg posReg];
          let cvtReg = 
            codegen#convert 
              ~destType:eltPtxT 
              ~srcType:storageT  
              ~srcReg:storageReg in 
          codegen#emit [mov eltPtxT resultReg cvtReg] 
        
        | ImpInferMemspace.SharedVec _ -> 
           failwith "shared memory space not implemented for imp->ptx stores" 
        )      
      
  | Imp.Set (id,rhs) -> 
      let dynT = PMap.find id tyMap in 
      let reg = codegen#declare_local id dynT in
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
        codegen#convert 
          ~destType:rhsStorageT
          ~srcType:rhsPtxT  
          ~srcReg:rhsReg in  
      let base = codegen#get_data_reg id in
      codegen#emit [comment "calculating storage index"];
      let idxReg, idxT = match idxRegs, idxTypes with 
        | [idxReg], [idxT] -> idxReg, idxT
        | _ -> failwith "ptx->imp only supports 1 dimensional array stores"
      in 
      (match PMap.find id memspaceMap with 
      | ImpInferMemspace.GlobalVec rank ->
        let idxReg' = 
          codegen#convert 
            ~destType:PtxType.U64
            ~srcType:idxT
            ~srcReg:idxReg in
        (* get offset as product of index and storage bytesize *)   
        let offset = codegen#fresh_reg PtxType.U64 in  
        codegen#emit [
          mul_lo PtxType.U64 offset idxReg' (int $ PtxType.nbytes rhsStorageT)
        ]; 
        (* compute linear offset into data array as sum of base and offset *) 
        
        let address = codegen#fresh_reg U64 in  
        codegen#emit [add U64 address base offset;    
                      st_global rhsStorageT address rhsRegCvt]
      
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
  debug "[translate_kernel] start\n%!";
  let _ = Array.map2 codegen#declare_local impfn.local_ids impfn.local_types in
  debug "[translate_kernel] declare local\n%!";
  let _ = Array.map2 codegen#declare_arg impfn.input_ids impfn.input_types in
  debug "[translate_kernel] declare arg\n%!";
  let _ = Array.map2 codegen#declare_arg impfn.output_ids impfn.output_types in
  debug "[translate_kernel] declare output\n%!";
  let memspaceMap = ImpInferMemspace.analyze_function impfn in
  debug "[translate_kernel] infer\n%!";
  let tyMap = impfn.tenv in 
  gen_block tyMap memspaceMap codegen impfn.body; 
  codegen#run_rewrite_pass PtxSimplify.simplify;   
  codegen#finalize_kernel



                  
(* 

        
and gen_array_load baseReg idxReg dynT  = 
  (* size of a Pred is actually the size of the short which holds its
     logical value 
  *) 
  let gpuT = PtxType.of_dyn_type dynT in 
  let gpuStorageT = PtxType.storage_of_dyn_type dynT in 
  let regs = GpuCodegen.get_regs U64 3 in
  let resultReg = GpuCodegen.get_reg gpuStorageT in
  GpuCodegen.emit [ 
    comment "Loading from array";
    cvt U64 U32 regs.(0) idxReg;            
      mul_lo U64 regs.(1) regs.(0) (const $ PtxType.nbytes gpuStorageT);
    add U64 regs.(2) baseReg regs.(1);    
    ld_global gpuStorageT resultReg regs.(2);
  ];
  (* might do nothing if gpuStorageT = gpuT *) 
  GpuCodegen.convert ~srcType:gpuStorageT ~destType:gpuT resultReg 
     
and gen_array_store baseReg idxReg rhsReg dynT =
  let gpuT = PtxType.of_dyn_type dynT in 
  let gpuStorageT = PtxType.storage_of_dyn_type dynT in 
  let regs = GpuCodegen.get_regs U64 3 in 
  GpuCodegen.emit [ 
    comment "Storing into array";
    cvt U64 U32 regs.(0) idxReg;      
    mul_lo U64 regs.(1) regs.(0) (const $ PtxType.nbytes gpuStorageT);
    add U64 regs.(2) baseReg regs.(1);    
  ];
  let rhsReg' = GpuCodegen.convert ~srcType:gpuT ~destType:gpuStorageT rhsReg in
  GpuCodegen.emit [st_global gpuStorageT regs.(2) rhsReg']
  *)      
      
(*| Imp.If (cond, tBlock, fBlock) -> 
  
  | Imp.SetIdx (id, idx, rhs) ->   
  *) 
 