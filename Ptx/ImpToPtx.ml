(* pp: -parser o pa_macro.cmo *)

open Base 
open Imp
open Ptx
open PtxVal 
open PtxType
open PtxCodegen
open Printf 


  
let num_to_ptx_const = function 
  | PQNum.Char c -> int $  Char.code c
  | PQNum.Int32 i -> int64 (Int64.of_int32 i)
  | PQNum.Int64 i -> int64 i
  | PQNum.Float32 f
  | PQNum.Float64 f -> float f
  | PQNum.Bool b -> int64 $ if b then 1L else 0L
  | PQNum.Inf t when DynType.is_floating t -> float max_float
  | PQNum.NegInf t when DynType.is_floating t -> float (-. max_float) 
  | PQNum.Inf _ -> int max_int 
  | PQNum.NegInf _ -> int min_int    
  | n -> 
      failwith $ Printf.sprintf "Can't convert %s to PTX"
        (PQNum.num_to_str n)


let prim_to_ptx_op codegen destReg args op t =
  match op, t with 
    | Prim.Abs, _ -> 
        codegen#emit [mkop (Unop(Abs,t)) (destReg::args)] 
    | Prim.Neg,_ -> 
        codegen#emit [mkop (Unop(Neg,t)) (destReg::args)] 
    | Prim.Not, Pred -> 
        codegen#emit [mkop (Unop(Not,Pred)) (destReg::args)] 
    | Prim.Sqrt,F64 ->  
        codegen#emit [mkop (Unop(Sqrt RN,F64)) (destReg::args)]
    | Prim.Sqrt, F32 -> 
        codegen#emit [mkop (Unop(Sqrt Approx,F32)) (destReg::args)]
    | Prim.Reciprocal, F32 -> 
        codegen#emit [mkop (Unop(Rcp Approx,F32)) (destReg::args)]
    | Prim.Reciprocal, F64 -> 
        codegen#emit [mkop (Unop(Rcp RN,F64)) (destReg::args)]
    | Prim.Exp2,F32 -> 
        codegen#emit [mkop (Unop(Ex2 Approx,F32)) (destReg::args)]
    | Prim.Lg2, F32 -> 
        codegen#emit [mkop (Unop(Lg2 Approx,F32)) (destReg::args)]
    | Prim.Ln, F32  ->
        let tmp1 : PtxVal.value  = codegen#fresh_reg F32 in
        let tmp2 : PtxVal.value = codegen#fresh_reg F32 in
        codegen#emit [
          mov tmp1 (FloatConst 0.693147181);
          mkop (Unop (Lg2 Approx, F32)) [tmp2; (List.hd args)];
          mul F32 destReg tmp2 tmp1;
        ]  
    | Prim.Ln, _ -> 
        let downCvt = codegen#fresh_reg F32 in 
        let tmp1 = codegen#fresh_reg F32 in 
        let tmp2 = codegen#fresh_reg F32 in
        let f32Result = codegen#fresh_reg F32 in
        codegen#emit [  
          cvt ~t1:F32 ~t2:t ~dest:downCvt ~src:(List.hd args);  
          mov tmp1 (FloatConst 0.693147181);
          mul F32 f32Result tmp2 tmp1;
          mkop (Unop (Lg2 Approx, F32)) [tmp2; downCvt];
          cvt ~t1:t ~t2:F32 ~dest:destReg ~src:f32Result
        ]
    | Prim.Exp, F32 ->
        let tmp1 = codegen#fresh_reg F32 in
        let tmp2 = codegen#fresh_reg F32 in
        codegen#emit [
          mov tmp1 (FloatConst 1.44269504);
          mul F32 tmp2 (List.hd args) tmp1;
          mkop  (Unop (Ex2 Approx, F32)) [destReg; tmp2]
        ]
    | Prim.Exp, _ -> 
        let downCvt = codegen#fresh_reg F32 in 
        let tmp1 = codegen#fresh_reg F32 in 
        let tmp2 = codegen#fresh_reg F32 in
        let f32Result = codegen#fresh_reg F32 in
        codegen#emit [
          cvt ~t1:F32 ~t2:t ~dest:downCvt ~src:(List.hd args);  
          mov tmp1 (FloatConst 1.44269504);
          mul t tmp2 downCvt tmp1;
          mkop (Unop (Ex2 Approx,F32)) [f32Result; tmp2]; 
          cvt ~t1:t ~t2:F32 ~dest:destReg ~src:f32Result
        ]
    | Prim.Add, _ -> 
        codegen#emit [mkop (Binop(Add,t)) (destReg::args)]
    | Prim.Sub,_ -> 
        codegen#emit [mkop (Binop(Sub,t)) (destReg::args)]
    | Prim.Mult, F32
    | Prim.Mult, F64 -> 
        codegen#emit [mkop (Binop(FloatMul, t)) (destReg::args)]
    | Prim.Mult, _ -> 
        codegen#emit [mkop (Binop(IntMul Low, t)) (destReg::args)]
    | Prim.Div, F32 -> 
        codegen#emit [mkop (Binop (FloatDiv Approx, F32)) (destReg::args)]
    | Prim.Div, F64 -> 
        codegen#emit [mkop (Binop (FloatDiv RN, F64)) (destReg::args)]
    | Prim.Div, _ -> 
        codegen#emit [mkop (Binop (IntDiv, t)) (destReg::args)]
    | Prim.Lt, t when PtxType.is_unsigned t ->  
        codegen#emit [mkop (Binop(Setp LO,t)) (destReg::args)]
    | Prim.Lt, _ ->  
        codegen#emit [mkop (Binop(Setp LT,t)) (destReg::args)]
    | Prim.Gt, t when PtxType.is_unsigned t -> 
        codegen#emit [mkop (Binop(Setp HI,t)) (destReg::args)]
    | Prim.Gt, _ -> 
        codegen#emit [mkop (Binop(Setp GT,t)) (destReg::args)]
    | Prim.Gte, t when PtxType.is_unsigned t -> 
        codegen#emit [mkop (Binop(Setp HS,t)) (destReg::args)]
    | Prim.Gte, _ -> 
        codegen#emit [mkop (Binop(Setp GE,t)) (destReg::args)]
    | Prim.Lte, t when PtxType.is_unsigned t-> 
        codegen#emit [mkop (Binop(Setp LS,t)) (destReg::args)]
    | Prim.Lte, _ -> 
        codegen#emit [mkop (Binop(Setp LE,t)) (destReg::args)]
    | Prim.Eq, _ -> 
        codegen#emit [mkop (Binop(Setp EQ,t)) (destReg::args)]
    | Prim.Neq, _ -> 
        codegen#emit [mkop (Binop(Setp NE,t)) (destReg::args)]
    | Prim.And, Pred ->
        codegen#emit [mkop (Binop(And,Pred)) (destReg::args)]
    | Prim.Or, Pred -> 
        codegen#emit [mkop (Binop(Or,Pred)) (destReg::args)]
    | Prim.Min, _ -> 
        codegen#emit [mkop (Binop(Min,t)) (destReg::args)]      
    | Prim.Max, _ -> 
        codegen#emit [mkop (Binop(Max,t)) (destReg::args)]    
    | _ ->
      failwith $  
        Printf.sprintf "[ImpToPtx] operator %s not implemented for type %s: "
          (Prim.scalar_op_to_str op)
          (PtxType.to_str t)


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
      IFDEF DEBUG THEN 
        same_type ptxResultT destType;
        assert (ptxResultT = destType);
      ENDIF; 
      let ptxArgT = PtxType.of_dyn_type dynArgType in
      let args' = List.map (fun x -> translate_arg x ptxArgT) args in
      prim_to_ptx_op codegen destReg args' op ptxArgT 
      
   | Imp.Select(t, cond, trueExp, falseExp) -> 
      let t' = PtxType.of_dyn_type t in
      IFDEF DEBUG THEN 
        assert (t' = destType);
      ENDIF;   
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
          sprintf "[ImpToPtx] array index expected to be 32-bit, received: %s"
          (PtxType.to_str idxPtxT);
      let idxReg = translate_arg idx idxPtxT in
      let eltBytes =
        PtxType.nbytes
          (PtxType.storage_of_dyn_type (DynType.elt_type dynResultT))
      in
      let isShared = codegen#is_shared_ptr baseReg in
      if not isShared && not (codegen#is_global_array_ptr baseReg) &&
         not (codegen#is_tex baseReg) then
        failwith "[ImpToPtx] cannot generate indexing code: \"
                 base address register is not global, texture, or shared ptr"
      else
      let rank = codegen#get_array_rank baseReg in
      let address = codegen#compute_address baseReg eltBytes [|idxReg|] in
      (* indexing into a 1D array yields a scalar *)  
      if rank = 1 then (
        let gpuStorageT = PtxType.storage_of_dyn_type dynResultT in
        let storageReg = codegen#fresh_reg gpuStorageT in
        if isShared then
          codegen#emit [ld_shared gpuStorageT storageReg address]
        else if codegen#is_tex baseReg then begin
	        let idxRegs = codegen#fresh_regs idxPtxT 3 in
	        for i = 0 to 2 do
	          codegen#emit[mov ~ty:idxPtxT idxRegs.(i) (int 0)]
	        done;
	        let gpuStorageT = PtxType.storage_of_dyn_type dynResultT in
	        let rsltRegs = codegen#fresh_regs gpuStorageT 3 in
          let texRef = codegen#get_tex_ref baseReg in
          match codegen#get_tex_geom texRef with 
            | Ptx.Tex1D -> 
              let offset = codegen#convert_fresh idxPtxT address in
	            codegen#emit [tex Ptx.Tex1D gpuStorageT texRef
	                          storageReg rsltRegs.(0) rsltRegs.(1) rsltRegs.(2)
	                          offset idxRegs.(0) idxRegs.(1) idxRegs.(2)]
            | Ptx.Tex2D -> 
               (* NOTE: address is not used here *) 
               let xCoord = codegen#convert_fresh idxPtxT idxReg in 
               let yCoord = codegen#convert_fresh idxPtxT baseReg in 
               codegen#emit [tex Ptx.Tex2D gpuStorageT texRef
                              storageReg rsltRegs.(0) rsltRegs.(1) rsltRegs.(2)
                              xCoord yCoord idxRegs.(1) idxRegs.(2)]
        end
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
  | Imp.Idx(_, _) -> failwith "[ImpToPtx] attempted to index into non-array"
  | Imp.Cast(tNew, x) ->
      let tNewPtx = PtxType.of_dyn_type tNew in 
      assert (tNewPtx = PtxVal.type_of_var destReg);
      let tOld = x.exp_type in  
      let tOldPtx = PtxType.of_dyn_type tOld in  
      let x' = translate_arg x tOldPtx in
      codegen#convert ~destReg ~srcVal:x' 
  
  (* Remember: Indexing is base 0!!! *) 
  | DimSize(dim, {Imp.exp=Var id}) ->
      let arrayReg = codegen#imp_reg id in 
      if codegen#is_shared_ptr arrayReg then 
        let dims = codegen#get_shared_dims arrayReg in 
        assert (dim < Array.length dims);
        let size = dims.(dim) in 
        codegen#emit [mov destReg (int size)]
     else if codegen#is_global_array_ptr arrayReg then 
        let rank = codegen#get_global_array_rank arrayReg in 
        assert (dim <= rank);
        let shapeReg = codegen#get_shape_reg arrayReg in
        codegen#emit [ld_global ~offset:(dim*4) S32 destReg shapeReg] 
     else failwith "[ImpToPtx] attempting to get DimSize of a scalar"
  
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
  (*
  IFDEF DEBUG THEN
    codegen#emit [comment (Imp.stmt_to_str stmt)];
    Printf.printf "[ImpToPtx] translating statement %s\n" 
      (String.escaped (String.abbrev (Imp.stmt_to_str stmt) 50))
  ENDIF;
  *) 
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
        failwith "[ImpToPtx] Cannot set an array slice"
      else if rank < numIndices then 
        failwith $ 
           Printf.sprintf 
             "[ImpToPtx] setidx expected %d index, received %d: %s"
             rank
             numIndices 
             (Imp.stmt_to_str stmt)
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
  | Imp.Comment str -> 
      IFDEF DEBUG THEN 
        codegen#emit [comment ("Imp comment: " ^ str)];
      ENDIF 
      
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
  (*
  IFDEF DEBUG THEN 
    print_string "\n[ImpToPtx] ***** started translation ****\n";
    print_string (Imp.fn_to_str impfn);
    print_string "\n";
    print_string "\n[ImpToPtx] ******************************\n";
  ENDIF;
  *)
  let default_space t = 
    if DynType.is_scalar t then PtxVal.PARAM else PtxVal.GLOBAL 
  in 
  let inputSpaces = match input_spaces with
    | _ -> Array.map default_space impfn.input_types 
    | Some inputSpaces -> inputSpaces  
  in 
  let codegen = new PtxCodegen.ptx_codegen in
  Array.iter3 
    codegen#declare_input 
    impfn.input_ids 
    impfn.input_types 
    inputSpaces
  ;
  Array.iter2 codegen#declare_output impfn.output_ids impfn.output_types; 
  
  (* declare all local variables as:
      1) a scalar register 
      2) a local array for which you receive a pointer 
      3) a shared array 
  *)
  let register_local id  = 
    let t = Hashtbl.find impfn.types id in
    let dims = Hashtbl.find_default impfn.sizes id [] in  
    IFDEF DEBUG THEN 
      let shapeRank = List.length dims in 
      let typeRank = DynType.nest_depth t in 
      if shapeRank <> typeRank then 
        failwith $ 
          Printf.sprintf 
            "[ImpToPtx] Incorrect rank for imp variable %s : %s with shape %s"
            (ID.to_str id)
            (DynType.to_str t)
            (SymbolicShape.shape_to_str dims)
    ENDIF;  
    let ptxVal = 
      if DynType.is_scalar t then codegen#declare_local id t
      else (
        assert (Hashtbl.mem impfn.array_storage id); 
        match Hashtbl.find impfn.array_storage id with 
        | Shared ->
          (* since dims are all constant, evaluate them to ints *) 
          let intDims = 
            List.map (ShapeEval.eval_exp_as_int ID.Map.empty) dims 
          in  
          codegen#declare_shared_vec id (DynType.elt_type t) intDims
        | Private ->  codegen#declare_storage_arg id t   
        | _ -> codegen#declare_local id t 
      )
    in  
    (*
    IFDEF DEBUG THEN 
      Printf.printf "[ImpToPtx] declaring imp var %s : %s as %s\n"
        (ID.to_str id)
        (DynType.to_str t);
        (codegen#value_to_str ptxVal);
    ENDIF;
    *)
    ()
        
  in    
  MutableSet.iter register_local impfn.local_id_set; 
  gen_block codegen impfn.body; 
  codegen#finalize_kernel
