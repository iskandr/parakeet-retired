(* pp: -parser o pa_macro.cmo *)

open Base
open Bigarray
open HostVal
open Printf 
open GpuVal 

module type GPU_RUNTIME_PARAMS = sig 
  val fnTable : FnTable.t
  val memState : MemoryState.t 
end 

type value = GpuVal.gpu_val
type values = value list 
type adverb_impl = SSA.fundef -> values -> values -> Type.t list -> values
type simple_array_op_impl = values -> Type.t list -> values 
exception InvalidGpuArgs


type types = Type.t array 
type spaces = PtxVal.ptx_space array 
type layouts = GpuVal.data_layout array 
type code_cache_key = FnId.t * types * layouts * spaces  

type code_cache_entry = {
  imp_source : Imp.fn;
  cc : PtxCallingConventions.calling_conventions;
  cuda_module : Cuda.cuda_module
} 

type code_cache = (code_cache_key, code_cache_entry) Hashtbl.t

let sizeof ty shape = Type.sizeof (Type.elt_type ty) * Shape.nelts shape

module Mk(P : GPU_RUNTIME_PARAMS) = struct 

  let assign_input_spaces inputTypes inputShapes =
    let inputRanks = Array.map Shape.rank inputShapes in 
    let totalRank = Array.fold_left (+) 0 inputRanks in 
    let totalConstantMem = 0 in (*HardwareInfo.device_info.(0).total_constant_mem*)
    let availableConstantMem = totalConstantMem - totalRank * 4  in
    let nInputs = Array.length inputTypes in
    let inputSpaces = Array.create nInputs PtxVal.GLOBAL in   
    (* assume we're always using device 0 *)
    (* we're only allowed to assign one variable to constant memory *)
    let haveConstantInput = ref false in 
       
    for i = 0 to nInputs - 1 do
      let t = inputTypes.(i) in 
      let eltT = Type.elt_type t in 
      let eltSize = Type.sizeof eltT in
      let s = inputShapes.(i) in
      let nelts = Shape.nelts s in  
      let r = inputRanks.(i) in
      let currBytes = nelts * eltSize in 
      if Type.is_scalar t then 
        inputSpaces.(i) <- PtxVal.PARAM
      else if not !haveConstantInput && 
              currBytes < availableConstantMem then (
        inputSpaces.(i) <- PtxVal.CONST; 
        haveConstantInput := true 
      )
      (* only use 1D textures *) 
      else if (r = 1 || r =2) && eltSize = 4 && nelts < 134217728  then
        inputSpaces.(i) <- PtxVal.TEX 
      ; 
      IFDEF DEBUG THEN 
        Printf.printf 
          "[Input Space] Input #%d type=%s, shape=%s, eltsz=%d, space=%s\n"
          i
          (Type.to_str t)
          (Shape.to_str s)
          eltSize 
          (PtxVal.ptx_space_to_str inputSpaces.(i))
      ENDIF; 
    done; 
    inputSpaces
  
  
  
  (**********************************************************
        FLIP DATA LAYOUT FROM ROW MAJOR TO COLUMN MAJOR
   **********************************************************)
    let flip_data_layout (inputVec : GpuVal.gpu_vec) =
      (* TODO: For now, this _only_ supports flipping 2D stuff *)
      Timing.start Timing.gpuFlipAlloc;
      let inputPtr = inputVec.vec_ptr in 
      let inputShape = inputVec.vec_shape in 
      let width = Shape.get inputShape 1 in 
      let height = Shape.get inputShape 0 in 
      let flipPtr = MemoryState.alloc_gpu P.memState inputVec.vec_nbytes in 
      Timing.stop Timing.gpuFlipAlloc;
      Timing.start Timing.gpuFlip;
      (match Type.elt_type inputVec.vec_t with
        | Type.Int32T ->
            Kernels.flip_int_2D inputPtr height width flipPtr
        | Type.Float32T ->
            Kernels.flip_float_2D inputPtr height width flipPtr
        | _ -> failwith "Flip only supported for 2D int and float"
      );
      Timing.stop Timing.gpuFlip;
      inputVec.vec_col_major <- Some flipPtr;  
      flipPtr
  
  (*************************************************************
       CONSTRUCT KERNEL PARAMS USING PTX CALLING CONVENTIONS
   *************************************************************)
  
  let copy_constant modulePtr gpuPtr nbytes offset = 
    Cuda.cuda_memcpy_device_to_symbol modulePtr "constants" gpuPtr nbytes offset 
    
  let get_column_major_ptr = function 
    | GpuVal.GpuArray gpuVec -> 
        if Option.is_some gpuVec.vec_col_major  then 
          Option.get gpuVec.vec_col_major
        else flip_data_layout gpuVec 
    | GpuVal.GpuScalar _ -> failwith "can't flip a scalar"
     
  (* any given gpu argument might either get placed into the array 
     of explicit args or it might become an implicit argument via a
     global texture
  *) 
      
  let arg_helper modulePtr inputVal = function
    | PtxCallingConventions.ScalarInput ->
        [CudaModule.GpuScalarArg(GpuVal.get_scalar inputVal)]
    | PtxCallingConventions.GlobalOutput shapeOffset -> 
        let shapePtr = GpuVal.get_shape_ptr inputVal in 
        let shapeBytes = GpuVal.get_shape_nbytes inputVal in
        copy_constant modulePtr shapePtr shapeBytes shapeOffset; 
        let dataPtr = GpuVal.get_ptr inputVal in 
        let dataBytes = GpuVal.get_nbytes inputVal in 
        [CudaModule.GpuArrayArg(dataPtr, dataBytes)] 
    | PtxCallingConventions.GlobalInput (dataLayout, shapeOffset) ->
        let shapePtr = GpuVal.get_shape_ptr inputVal in 
        let shapeBytes = GpuVal.get_shape_nbytes inputVal in
        copy_constant modulePtr shapePtr shapeBytes shapeOffset; 
        let dataPtr = match dataLayout with 
          | GpuVal.RowMajor -> GpuVal.get_ptr inputVal  
          | GpuVal.ColumnMajor -> get_column_major_ptr inputVal
        in 
        let dataBytes = GpuVal.get_nbytes inputVal in 
        [CudaModule.GpuArrayArg(dataPtr, dataBytes)]
    | PtxCallingConventions.TextureInput (texName, geom, layout,shapeOffset) ->
        let texRef = Cuda.cuda_module_get_tex_ref modulePtr texName in
        let inputShape = GpuVal.get_shape inputVal in
        let inputPtr = match layout with 
          | GpuVal.RowMajor -> GpuVal.get_ptr inputVal 
          | GpuVal.ColumnMajor -> get_column_major_ptr inputVal  
        in
        let channelFormat = 
          Cuda.infer_channel_format 
            (Type.elt_type $ GpuVal.get_type inputVal)
        in 
        (match geom with
          | Ptx.Tex1D ->
              assert (Shape.rank inputShape < 3);
              Cuda.cuda_bind_texture_1d
                texRef 
                inputPtr 
                (GpuVal.get_nbytes inputVal) 
                channelFormat
          | Ptx.Tex2D ->
              (* BROKEN, Imp2Ptx should turn all textured inputs 
                 into Tex1D for now 
               *) 
              (* TODO: Need to set length/width to be in _BYTES_ *)
              assert (Shape.rank inputShape = 2); 
              Cuda.cuda_bind_texture_2d_std_channel
                texRef
                inputPtr
                (Shape.get inputShape 1)
                (Shape.get inputShape 0)
                channelFormat  
          | Ptx.Tex3D -> failwith "3D textures not yet implemented"
        );
        let shapePtr = GpuVal.get_shape_ptr inputVal in 
        let shapeBytes = GpuVal.get_shape_nbytes inputVal in
        copy_constant modulePtr shapePtr shapeBytes shapeOffset; 
        []    
    | PtxCallingConventions.ConstantInput (shapeOffset, dataOffset) ->
        let shapePtr = GpuVal.get_shape_ptr inputVal in 
        let shapeBytes = GpuVal.get_shape_nbytes inputVal in
        copy_constant modulePtr shapePtr shapeBytes shapeOffset; 
        let dataPtr = GpuVal.get_ptr inputVal in 
        let dataBytes = GpuVal.get_nbytes inputVal in
        copy_constant modulePtr dataPtr dataBytes dataOffset;
        []   
(*
  let process_private_array nThreads impfn shapeEnv id storage env = 
    if storage = Imp.Private then
      let ty = Hashtbl.find impfn.Imp.types id in  
      let privateShape = ID.Map.find id shapeEnv in
      let globalShape = Shape.append_dim nThreads privateShape in
      let vec = MemoryState.mk_gpu_vec ~refcount:1 P.memState ty globalShape in  
      let gpuShapePtr = vec.GpuVal.vec_shape_ptr in 
      let gpuShapeBytes = vec.GpuVal.vec_shape_nbytes in    
      let gpuArgs =[
        CudaModule.GpuArrayArg(vec.GpuVal.vec_ptr, vec.GpuVal.vec_nbytes);
        CudaModule.GpuArrayArg(gpuShapePtr, gpuShapeBytes)
      ]
      in 
      ID.Map.add id gpuArgs env
    else env  
*)
  let extend_env cc modulePtr env id gpuVal =
      let location = ID.Map.find id cc.PtxCallingConventions.data_locations in
      let gpuArgs = arg_helper modulePtr gpuVal location in
      ID.Map.add id gpuArgs env


  let rec create_output_vals impfn shapeEnv outputOrder = function 
    | [] -> [] 
    | id::ids ->  
      IFDEF DEBUG THEN 
        assert (Hashtbl.mem impfn.Imp.types id);
        assert (ID.Map.mem id shapeEnv); 
      ENDIF;
      let ty = Hashtbl.find impfn.Imp.types id in
      let shape = ID.Map.find id shapeEnv in
      IFDEF DEBUG THEN 
        assert (Type.is_vec ty);
        assert (Shape.rank shape > 0);
      ENDIF; 
      let vec = MemoryState.mk_gpu_vec ~refcount:1 P.memState ty shape in
      let gpuVal = GpuVal.GpuArray vec in  
      DynArray.add outputOrder gpuVal;
      gpuVal :: (create_output_vals impfn shapeEnv outputOrder ids) 
       
  let create_args 
        (modulePtr : Cuda.CuModulePtr.t) 
        (nThreads : int)
        (impfn : Imp.fn) 
        (cc : PtxCallingConventions.calling_conventions) 
        (inputs: GpuVal.gpu_val list) 
        : CudaModule.gpu_arg array * GpuVal.gpu_val list  =
    let inputShapes = List.map GpuVal.get_shape inputs in 
    let shapeEnv = ShapeEval.eval_imp_shape_env impfn inputShapes in
    let outputOrder = DynArray.create() in 
    let outputIdList = Array.to_list impfn.Imp.output_ids in  
    let outputs = 
      create_output_vals impfn shapeEnv outputOrder outputIdList 
    in 
    let combinedIds =  
      Array.append impfn.Imp.input_ids impfn.Imp.output_ids 
    in 
    let combinedParams = Array.of_list (inputs @ outputs) in 
    let argEnv = ref ID.Map.empty in 
    for i = 0 to Array.length combinedParams - 1 do
      let gpuVal = combinedParams.(i) in  
      let id = combinedIds.(i) in 
      argEnv :=  extend_env cc modulePtr !argEnv id gpuVal 
    done;  
    let paramsArray = DynArray.create() in
    let process_param id =
      let args = ID.Map.find id !argEnv in
      IFDEF DEBUG THEN 
          Printf.printf "[GpuRuntime] Registering GPU params for %s = [%s]\n%!" 
            (ID.to_str id)
            (String.concat ", "  (List.map CudaModule.gpu_arg_to_str args)) 
          ; 
      ENDIF;
      List.iter (DynArray.add paramsArray) args
    in  
    Array.iter process_param cc.PtxCallingConventions.param_order;
    (DynArray.to_array paramsArray), (DynArray.to_list outputOrder)



  (**********************************************************
                           MAP 
   **********************************************************)
  
  let map_id_gen = mk_gen ()
  let mapThreadsPerBlock = 256 
  
  let compile_map payload 
        (closureTypes : Type.t array) 
        (argTypes : Type.t array) 
        (retTypes : Type.t array)
        (dataLayouts : GpuVal.data_layout array) 
        (inputSpaces : PtxVal.ptx_space array) =
    (* converting payload to Imp *) 
    let impPayload = SSA_to_Imp.translate_fundef P.fnTable payload in
    (* generating Imp kernel w/ embedded payload *)
    let impfn =
      ImpMapTemplate.gen_map 
        impPayload
        mapThreadsPerBlock 
        closureTypes 
        argTypes 
        retTypes
    in
    let kernel, cc = ImpToPtx.translate_kernel impfn ~dataLayouts inputSpaces in
    let kernelName = "map_kernel" ^ (string_of_int (map_id_gen())) in
    let cudaModule = 
      CudaModule.cuda_module_from_kernel_list
        [kernelName, kernel] 
        mapThreadsPerBlock
    in
    {imp_source=impfn; cc=cc; cuda_module=cudaModule}

  let mapCache : code_cache = Hashtbl.create 127

  
  let decide_map_input_layout = function 
    | Type.VecT (Type.VecT Type.Float32T)
    | Type.VecT (Type.VecT Type.Int32T) -> GpuVal.ColumnMajor 
    | _ -> GpuVal.RowMajor 

  let map ~payload ~closureArgs ~args =
    let closureTypes = Array.of_list (List.map GpuVal.get_type closureArgs) in
    let closureShapes = Array.of_list (List.map GpuVal.get_shape closureArgs) in
    let inputTypesList = List.map GpuVal.get_type args in
    let inputTypes = Array.of_list inputTypesList in 
    let inputShapes = Array.of_list (List.map GpuVal.get_shape args) in 
    let combinedInputTypes = Array.append closureTypes inputTypes in  
    let combinedInputShapes = Array.append closureShapes inputShapes in
    let combinedInputSpaces = 
      assign_input_spaces combinedInputTypes combinedInputShapes
    in
    let outputTypes = 
      Array.of_list 
        (List.map (fun t -> Type.VecT t) payload.SSA.fn_output_types)
    in
    let closureLayouts = Array.map (fun _ -> GpuVal.RowMajor) closureTypes in 
    let inputLayouts = Array.map decide_map_input_layout inputTypes in 
    let dataLayouts = Array.append closureLayouts inputLayouts in   
    let cacheKey = 
      payload.SSA.fn_id, combinedInputTypes, dataLayouts, combinedInputSpaces 
    in  
    let {imp_source=impKernel; cc=cc; cuda_module=cudaModule} = 
      if Hashtbl.mem mapCache cacheKey then Hashtbl.find mapCache cacheKey
      else (
        let entry = 
          compile_map 
            payload 
            closureTypes 
            inputTypes 
            outputTypes
            dataLayouts  
            combinedInputSpaces 
        in
        Hashtbl.add mapCache cacheKey entry; 
        entry
    )  
    in 
    (* create one CUDA thread per every input element *) 
    assert (List.length cudaModule.Cuda.kernel_names = 1); 
    let fnName = List.hd cudaModule.Cuda.kernel_names in
    let maxShape = 
      match Shape.max_shape_list (List.map GpuVal.get_shape args) with 
      | Some maxShape -> maxShape  
      | None -> assert false
    in 
    let outputElts = Shape.nelts maxShape in
    let gridParams = 
      match HardwareInfo.get_grid_params 
              ~device:0
              ~block_size:cudaModule.Cuda.threads_per_block outputElts
      with
        | Some gridParams -> gridParams
        | None ->
        failwith (sprintf "Unable to get launch params for %d elts" outputElts)
    in
    let modulePtr = cudaModule.Cuda.module_ptr in 
    Timing.start Timing.gpuMapAlloc;
    let combinedArgs = closureArgs @ args in 
    let paramsArray, outputVals =
      create_args modulePtr outputElts impKernel cc combinedArgs
    in
    Timing.stop Timing.gpuMapAlloc;
    Timing.start Timing.gpuMap;    
    CudaModule.launch_ptx
      cudaModule.Cuda.module_ptr 
      fnName 
      paramsArray 
      gridParams
    ;
    Timing.stop Timing.gpuMap; 
    IFDEF DEBUG THEN
      print_string "\n --- MAP ---\n";
      let sep = ";   " in 
      let inputSpaceString = 
        String.concat sep 
          (List.map PtxVal.ptx_space_to_str (Array.to_list combinedInputSpaces))
      in
      Printf.printf "[GpuRuntime] MAP memory spaces:%s\n" inputSpaceString;  
      let layoutString = 
        String.concat sep 
          (List.map GpuVal.data_layout_to_str (Array.to_list dataLayouts))
      in
      Printf.printf "[GpuRuntime] MAP data layouts: %s\n" layoutString; 
      let closureArgString = 
        String.concat sep (List.map GpuVal.to_str closureArgs) 
      in
      Printf.printf "[GpuRuntime] MAP closureArgs: %s\n" closureArgString;  
      let inputArgString = 
        String.concat sep (List.map GpuVal.to_str args)
      in
      Printf.printf "[GpuRuntime] MAP inputs: %s\n" inputArgString;
      let outputString = 
        String.concat sep (List.map GpuVal.to_str outputVals)
      in
      Printf.printf "[GpuRuntime] MAP outputs: %s\n" outputString      
       
    ENDIF;
    outputVals


  (**********************************************************
                           REDUCE 
   **********************************************************)
  let compile_reduce payload inputSpaces =
    let threadsPerBlock = 256 in
    (* assuming accumulator and vector element types are the same *) 
    let inType = List.hd payload.SSA.fn_output_types in
    (* this is very hackish, but we know that the GPU can compile *)
    (* a better kernel for maps nested within a reduce, so we extract*)
    (* the function being mapped and pass it as if it were the *)
    (* direct argument to reduce. This relies on the code generator for*)
    (* the kernel doing something smart with 2D arguments. BEWARE! *) 
            
    let payload = match SSA.extract_nested_map_fn_id payload with 
      | Some fnId -> FnTable.find fnId P.fnTable  
      | None -> payload 
    in  
    let impPayload = 
       SSA_to_Imp.translate_fundef P.fnTable payload 
    in
    let impfn =
      ImpReduceTemplate.gen_reduce_2d_capable inType impPayload threadsPerBlock  
    in
    (*let retTypes = impfn.Imp.output_types in*) 
    let ptx, cc = ImpToPtx.translate_kernel impfn inputSpaces in
    let reducePrefix = "reduce_kernel" in
    let name = reducePrefix ^ (string_of_int (ID.gen())) in
    let cudaModule = 
      CudaModule.cuda_module_from_kernel_list [name,ptx] threadsPerBlock
    in 
    {imp_source=impfn; cc=cc; cuda_module=cudaModule} 

  let reduceCache : code_cache  = Hashtbl.create 127 

  let reduce
      ~(init: SSA.fundef) ~(initClosureArgs:values)
      ~(payload : SSA.fundef) ~(payloadClosureArgs:values)
      ~(initArgs:values) ~(args:values) : values  =
  (*let initTypes = List.map GpuVal.get_type initArgs in*)  
  let vecTypes = Array.of_list (List.map GpuVal.get_type args) in
  let vecShapes = Array.of_list (List.map GpuVal.get_shape args) in  
  let inputSpaces = assign_input_spaces vecTypes vecShapes in 
  let cacheKey = payload.SSA.fn_id, vecTypes, [||], inputSpaces  in 
  let {imp_source=impKernel; cc=cc; cuda_module=compiledModule} =  
    if Hashtbl.mem reduceCache cacheKey then 
      Hashtbl.find reduceCache cacheKey
    else (
      let entry =  compile_reduce payload  inputSpaces in 
      Hashtbl.add reduceCache cacheKey entry; 
      entry
    )
  in 
  let threadsPerBlock = compiledModule.Cuda.threads_per_block in
  let outputTypes = payload.SSA.fn_output_types in 
  assert (List.length outputTypes = 1); 
  
  let fnName = match compiledModule.Cuda.kernel_names with 
    | [fnName] -> fnName
    | _ -> failwith "expect one reduce kernel" 
  in 
  let gpuVal = List.hd args in 
    (* WAYS THIS IS CURRENTLY WRONG: 
       - we are ignoring the initial value
       - we are only allowing reductions over a single array
       - in the 2D case, we only support embedded maps
       - DOESN'T SEND CLOSURE ARGS TO THE GPU! 
    *)
  
  let inShape = GpuVal.get_shape gpuVal in
  let numInputElts = Shape.get inShape 0 in
  let x_threads = 1 in
  let x_grid = 
    if Shape.rank inShape = 1 then 1 else (Shape.get inShape 1) / x_threads
  in
  let currInputElts = ref numInputElts in 
  IFDEF DEBUG THEN 
    print_string "\n --- REDUCE ---\n";
  ENDIF;
  let modulePtr = compiledModule.Cuda.module_ptr in 
  let inputArgs = ref [gpuVal] in 
  let iter = ref 0 in 
  while !currInputElts > 1 do
    iter := !iter + 1;  
    IFDEF DEBUG THEN 
      Printf.printf 
       "[GpuRuntime] REDUCE input (iter %d): %s\n"
       !iter 
       (GpuVal.to_str gpuVal)
      ;
    ENDIF;  
    let numOutputElts = safe_div !currInputElts (threadsPerBlock * 2) in
    Timing.start Timing.gpuReduceAlloc;   
    let args, outputsList =
      create_args modulePtr !currInputElts impKernel cc !inputArgs 
    in
    Timing.stop Timing.gpuReduceAlloc;
    let gridParams = {
      CudaModule.threads_x=x_threads; threads_y=256; threads_z=1;
      grid_x=x_grid; grid_y=numOutputElts;
    }
    in
    Timing.start Timing.gpuReduce; 
    CudaModule.launch_ptx compiledModule.Cuda.module_ptr fnName args gridParams;
    Timing.stop Timing.gpuReduce; 
    inputArgs := outputsList; 
    currInputElts := numOutputElts; 
  done; 
  let result = MemoryState.slice_gpu_val P.memState (List.hd !inputArgs) 0 in
  IFDEF DEBUG THEN
    Printf.printf "[GpuRuntime] REDUCE output: %s\n" (GpuVal.to_str result); 
  ENDIF;
  [result]
    

  (**********************************************************
                           ALLPAIRS 
   **********************************************************)
  let compile_all_pairs payload argTypes inputSpaces = 
    match argTypes with 
    | [t1; t2] ->
      let threadsPerBlock = 256 in
      let impPayload = SSA_to_Imp.translate_fundef P.fnTable payload in
      let retTypes =
        List.map 
          (fun t -> Type.VecT (Type.VecT t))
          payload.SSA.fn_output_types 
      in 
      let impfn =
        ImpAllPairsTemplate.gen_all_pairs_2d_naive impPayload t1 t2 retTypes
      in
		  let kernel, cc = ImpToPtx.translate_kernel impfn inputSpaces  in
      let allPairsPrefix = "all_pairs_kernel" in
      let name = allPairsPrefix ^ (string_of_int (ID.gen())) in
      let compiledModule =
        CudaModule.cuda_module_from_kernel_list [name, kernel] threadsPerBlock
      in
      {imp_source=impfn; cc=cc; cuda_module=compiledModule}
    | _ -> failwith "[compile_all_pairs] invalid argument types "

  let allPairsCache  = Hashtbl.create 127 

  let all_pairs ~(payload : SSA.fundef) ~payloadClosureArgs ~args =
    (* CLOSURE ARGS GET IGNORED! *)  
    let inputTypes = List.map GpuVal.get_type args in
    let inputShapesArr = Array.of_list (List.map GpuVal.get_shape args) in
    let inputSpaces =
       assign_input_spaces (Array.of_list inputTypes) inputShapesArr
    in  
    let cacheKey = payload.SSA.fn_id, inputTypes, inputSpaces in 
    let {imp_source=impKernel; cc=cc; cuda_module=compiledModule} = 
      if Hashtbl.mem allPairsCache cacheKey then 
        Hashtbl.find allPairsCache cacheKey
      else (
        let entry = compile_all_pairs payload inputTypes inputSpaces in 
        Hashtbl.add allPairsCache cacheKey entry;
        entry
      )
    in
    match compiledModule.Cuda.kernel_names, args with
    | _, [] | _, [_] | _, _::_::_::_ ->  
        failwith "[run_all_pairs] wrong number of arguments"
    | [], _ | _::_::_, _ ->
        failwith "[run_all_pairs] wrong number of functions" 
    | [fnName], [xGpu;yGpu] ->
      let xShape, yShape = GpuVal.get_shape xGpu, GpuVal.get_shape yGpu in
      let nx = Shape.get xShape 0 in
      let ny = Shape.get yShape 0 in
      let nthreads = nx * ny in
      let modulePtr = compiledModule.Cuda.module_ptr in  
      let paramsArray, outputVals =
        create_args modulePtr nthreads impKernel cc args in 
      let gridParams = {
          CudaModule.threads_x=16; threads_y=16; threads_z=1;
          grid_x=safe_div nx 16; grid_y=safe_div ny 16;
      }
      in
      CudaModule.launch_ptx
        compiledModule.Cuda.module_ptr fnName paramsArray gridParams;
      outputVals

  (**********************************************************
                          INDEX 
   **********************************************************)
  
  let index (inputVec : gpu_val) (indexVec : gpu_val) =
    let inputShape = GpuVal.get_shape inputVec in
    let ninputels = Shape.nelts inputShape in
    let nidxs = GpuVal.nelts indexVec in
    let ninputs = Shape.get inputShape 0 in
    let r = Shape.rank inputShape in
    let outputShape = Shape.create r in
    Shape.set outputShape 0 nidxs;
    if r > 2 or r < 1 then
      failwith "Index only works for 1D and 2D inputs for now";
    let vec_len = if r = 1 then 1
    else begin
      let len = Shape.get inputShape 1 in
      Shape.set outputShape 1 len;
      len
    end
    in
    let inputType = GpuVal.get_type inputVec in 
    let elType = Type.elt_type inputType in
    Timing.start Timing.gpuIndexAlloc; 
    let output = 
      MemoryState.mk_gpu_vec ~refcount:1 P.memState inputType outputShape 
    in
    Timing.stop Timing.gpuIndexAlloc; 
    let inputPtr = GpuVal.get_ptr inputVec in
    let indexPtr = GpuVal.get_ptr indexVec in
    Timing.start Timing.gpuIndex;
    Kernels.bind_index_idxs_tex indexPtr nidxs;
    begin match elType with
    | Type.Int32T -> begin
        Kernels.bind_index_int_vecs_tex inputPtr ninputels;
        Kernels.index_int ninputs vec_len nidxs output.vec_ptr;
        Kernels.unbind_index_int_vecs_tex ()
      end
    | Type.Float32T -> begin
        Kernels.bind_index_float_vecs_tex inputPtr ninputels;
        Kernels.index_float ninputs vec_len nidxs output.vec_ptr;
        Kernels.unbind_index_float_vecs_tex ()
      end
    | _ -> 
        failwith $ 
          Printf.sprintf  
            "[GpuRuntime] unsupported element type for indexing (%s)"
            (Type.to_str elType)
    end;
    Timing.stop Timing.gpuIndex;
    let gpuVal = GpuVal.GpuArray output in 
    IFDEF DEBUG THEN 
      print_string "\n --- INDEX ---\n";
      Printf.printf "[GpuRuntime] INDEX input 1: %s\n" (GpuVal.to_str inputVec); 
      Printf.printf "[GpuRuntime] INDEX input 2: %s\n" (GpuVal.to_str indexVec);
      Printf.printf "[GpuRuntime] INDEX output: %s\n" (GpuVal.to_str gpuVal); 
    ENDIF; 
    gpuVal 

  (**********************************************************
                          WHERE 
   **********************************************************)
    let where (binVec : gpu_val) =
      let binPtr = GpuVal.get_ptr binVec in
      let nelts = GpuVal.nelts binVec in
      let scanShape = GpuVal.get_shape binVec in
      let int32_vec_type = Type.VecT Type.Int32T in  
      Timing.start Timing.gpuWhereAlloc; 
      let scanInterm = 
        MemoryState.mk_gpu_vec ~refcount:1 P.memState int32_vec_type scanShape 
      in
      Timing.stop Timing.gpuWhereAlloc;
      Timing.start Timing.gpuWhere;   
      Thrust.thrust_prefix_sum_bool_to_int binPtr nelts scanInterm.vec_ptr;
      Timing.stop Timing.gpuWhere; 
      let resultLength = 
        Cuda.cuda_get_gpu_int_vec_elt scanInterm.vec_ptr (nelts - 1) 
      in
      
      let outputShape = Shape.create 1 in
      Shape.set outputShape 0 resultLength;
      IFDEF DEBUG THEN
        print_string "\n --- WHERE --- \n";
        Printf.printf "[GpuRuntime] WHERE input: %s\n" (GpuVal.to_str binVec);
        Printf.printf "[GpuRuntime] WHERE prefix scan: %s\n"
          (GpuVal.gpu_vec_to_str scanInterm);
        Printf.printf "[GpuRuntime] WHERE returned %d elements\n" resultLength;
      ENDIF; 
      Timing.start Timing.gpuWhereAlloc;
      let output = 
        MemoryState.mk_gpu_vec ~refcount:1 P.memState int32_vec_type outputShape 
      in
      Timing.stop Timing.gpuWhereAlloc;
      Timing.start Timing.gpuWhere; 
      Kernels.bind_where_tex scanInterm.vec_ptr nelts;
      Kernels.where_tex nelts output.vec_ptr; 
      Kernels.unbind_where_tex ();
      Timing.stop Timing.gpuWhere; 
      GpuVal.GpuArray output

  
(*
let init () =
  (* initialize GPU contexts and device info *)
  CudaModule.cuda_init();
  HardwareInfo.hw_init()
*)
  
  let shutdown () = ()
  (*
  for i = 0 to DynArray.length HardwareInfo.device_contexts - 1 do 
    Cuda.cuda_ctx_destroy (DynArray.get HardwareInfo.device_contexts i)
  done
  *)
  
end  
