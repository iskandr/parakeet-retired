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
type adverb_impl = SSA.fundef -> values -> values -> DynType.t list -> values
type simple_array_op_impl = values -> DynType.t list -> values 
exception InvalidGpuArgs
type code_cache_entry = {
  imp_source : Imp.fn;
  cc : PtxCallingConventions.calling_conventions;
  cuda_module : Cuda.cuda_module
}
type code_cache = (ID.t * DynType.t list, code_cache_entry) Hashtbl.t

let sizeof ty shape = DynType.sizeof (DynType.elt_type ty) * Shape.nelts shape

module Mk(P : GPU_RUNTIME_PARAMS) = struct 

  (* any given gpu argument might either get placed into the array 
     of explicit args or it might become an implicit argument via a
     global texture
  *) 
  let create_input_args modulePtr inputVal = function
    | PtxCallingConventions.ScalarInput ->
        [CudaModule.GpuScalarArg(GpuVal.get_scalar inputVal)]
    | PtxCallingConventions.GlobalInput ->
        [CudaModule.GpuArrayArg((GpuVal.get_ptr inputVal),
                                (GpuVal.get_nbytes inputVal));
         CudaModule.GpuArrayArg((GpuVal.get_shape_ptr inputVal),
                                (GpuVal.get_shape_nbytes inputVal))]
    | PtxCallingConventions.TextureInput (texName, geom) ->
      let texRef = Cuda.cuda_module_get_tex_ref modulePtr texName in
      let inputShape = GpuVal.get_shape inputVal in
      let inputPtr = GpuVal.get_ptr inputVal in
      let channelFormat = 
        Cuda.infer_channel_format 
          (DynType.elt_type $ GpuVal.get_type inputVal)
      in (
        match geom with
      | Ptx.Tex1D ->
        assert (Shape.rank inputShape < 3);
        Cuda.cuda_bind_texture_1d
          texRef inputPtr (GpuVal.get_nbytes inputVal) channelFormat
      | Ptx.Tex2D ->
        (* TODO: Need to set length/width to be in _BYTES_ *)
        assert (Shape.rank inputShape = 2); 
        Cuda.cuda_bind_texture_2d_std_channel
          texRef
          inputPtr
          (Shape.get inputShape 1)
          (Shape.get inputShape 0)
          channelFormat  
      | _ -> failwith "3D textures not yet implemented"
    );
    [CudaModule.GpuArrayArg((GpuVal.get_shape_ptr inputVal),
                            (GpuVal.get_shape_nbytes inputVal))]
  
  let process_private_array nThreads impfn shapeEnv id storage env = 
    if storage = Imp.Private then
      let ty = Hashtbl.find impfn.Imp.types id in  
      let privateShape = ID.Map.find id shapeEnv in
      let globalShape = Shape.append_dim nThreads privateShape in
      let vec = MemoryState.mk_gpu_vec P.memState ty globalShape in  
      let gpuShapePtr = vec.GpuVal.vec_shape_ptr in 
      let gpuShapeBytes = vec.GpuVal.vec_shape_nbytes in    
      let gpuArgs =[
        CudaModule.GpuArrayArg(vec.GpuVal.vec_ptr, vec.GpuVal.vec_nbytes);
        CudaModule.GpuArrayArg(gpuShapePtr, gpuShapeBytes)
      ]
      in 
      ID.Map.add id gpuArgs env
    else env  

  let process_input cc modulePtr env id gpuVal =
      let location = ID.Map.find id cc.PtxCallingConventions.data_locations in
      let gpuArgs = create_input_args modulePtr gpuVal location in
      ID.Map.add id gpuArgs env

  let process_output impfn shapeEnv outputMap env id  =
      IFDEF DEBUG THEN 
        assert (Hashtbl.mem impfn.Imp.types id);
        assert (ID.Map.mem id shapeEnv); 
      ENDIF;
      let ty = Hashtbl.find impfn.Imp.types id in
      let shape = ID.Map.find id shapeEnv in
      IFDEF DEBUG THEN 
        assert (DynType.is_vec ty);
        assert (Shape.rank shape > 0);
      ENDIF; 
      let vec = MemoryState.mk_gpu_vec P.memState ty shape in 
      DynArray.add outputMap (GpuVal.GpuArray vec);
      let gpuShapePtr = vec.GpuVal.vec_shape_ptr in 
      let gpuShapeBytes = vec.GpuVal.vec_shape_nbytes in  
      let args = [
        CudaModule.GpuArrayArg(vec.GpuVal.vec_ptr, vec.GpuVal.vec_nbytes);
        CudaModule.GpuArrayArg(gpuShapePtr, gpuShapeBytes)
      ] 
      in 
      ID.Map.add id args env 
  
  let create_args 
        (modulePtr : Cuda.CuModulePtr.t) 
        (nThreads : int)
        (impfn : Imp.fn) 
        (cc : PtxCallingConventions.calling_conventions) 
        (inputs: GpuVal.gpu_val list) 
        : CudaModule.gpu_arg array * GpuVal.gpu_val list  =
    let inputShapes = List.map GpuVal.get_shape inputs in 
    let shapeEnv = ShapeEval.eval_imp_shape_env impfn inputShapes in 
    let inputArray = Array.of_list inputs in
    let initEnv = 
      Array.fold_left2 
        (process_input cc modulePtr)
        ID.Map.empty 
        impfn.Imp.input_ids
        inputArray 
    in
    let outputMap = DynArray.create() in
    let valueEnv : CudaModule.gpu_arg list ID.Map.t  = 
      Array.fold_left
        (process_output impfn shapeEnv outputMap)
        initEnv 
        impfn.Imp.output_ids  
    in 
    let valueEnv' : CudaModule.gpu_arg list ID.Map.t = 
      Hashtbl.fold
        (process_private_array nThreads impfn shapeEnv)
        impfn.Imp.array_storage 
        valueEnv
    in 
    let paramsArray = DynArray.create() in
    let process_param id =
      let args = ID.Map.find id valueEnv' in
      IFDEF DEBUG THEN 
          Printf.printf "[GpuRuntime] Registering GPU param for %s = %s\n" 
            (ID.to_str id)
            (String.concat ", "  (List.map CudaModule.gpu_arg_to_str args)) 
          ; 
      ENDIF;
      List.iter (DynArray.add paramsArray) args
    in  
    Array.iter process_param cc.PtxCallingConventions.param_order;
    (DynArray.to_array paramsArray), (DynArray.to_list outputMap)


  (**********************************************************
                           MAP 
   **********************************************************)
  
  let map_id_gen = mk_gen ()
  let mapThreadsPerBlock = 256 
  let compile_map payload closureTypes argTypes retTypes =
    (* converting payload to Imp *) 
    let impPayload = SSA_to_Imp.translate_fundef P.fnTable payload in
    (* generating Imp kernel w/ embedded payload *)
    let impfn =
      ImpMapTemplate.gen_map
        impPayload
        mapThreadsPerBlock
        (Array.of_list closureTypes)
        (Array.of_list argTypes)
        (Array.of_list retTypes)
    in
    let kernel, cc = ImpToPtx.translate_kernel impfn in
    let kernelName = "map_kernel" ^ (string_of_int (map_id_gen())) in
    let cudaModule = CudaModule.cuda_module_from_kernel_list
      [kernelName, kernel] mapThreadsPerBlock
    in
    {imp_source=impfn; cc=cc; cuda_module=cudaModule}

  let mapCache : code_cache = Hashtbl.create 127
 
  let map ~payload ~closureArgs ~args =
    let closureTypes = List.map GpuVal.get_type closureArgs in 
    let inputTypes = List.map GpuVal.get_type args in
    let outputTypes = 
      List.map (fun t -> DynType.VecT t) payload.SSA.fn_output_types 
    in 
    let cacheKey = (payload.SSA.fn_id, closureTypes @ inputTypes) in  
    let {imp_source=impKernel; cc=cc; cuda_module=cudaModule} = 
      if Hashtbl.mem mapCache cacheKey then Hashtbl.find mapCache cacheKey
      else (
        let entry = compile_map payload closureTypes inputTypes outputTypes in
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
    match HardwareInfo.get_grid_params ~device:0
      ~block_size:cudaModule.Cuda.threads_per_block outputElts
    with
      | Some gridParams -> gridParams
      | None ->
        failwith (sprintf "Unable to get launch params for %d elts" outputElts)
  in
  let modulePtr = cudaModule.Cuda.module_ptr in 
  let paramsArray, outputVals =
    create_args modulePtr outputElts impKernel cc (closureArgs @ args)
  in  
  CudaModule.launch_ptx
    cudaModule.Cuda.module_ptr 
    fnName 
    paramsArray 
    gridParams
  ;
  IFDEF DEBUG THEN
    print_string "\n --- MAP ---\n";
    let sep = ";   " in 
    let closureArgString = 
      String.concat sep (List.map GpuVal.to_str closureArgs) 
    in  
    let inputArgString = 
      String.concat sep (List.map GpuVal.to_str args)
    in
    let outputString = 
      String.concat sep (List.map GpuVal.to_str outputVals)
    in
    Printf.printf "[GpuRuntime] MAP closureArgs: %s\n" closureArgString;      
    Printf.printf "[GpuRuntime] MAP inputs: %s\n" inputArgString;  
    Printf.printf "[GpuRuntime] MAP outputs: %s\n" outputString 
  ENDIF;
  outputVals


  (**********************************************************
                           REDUCE 
   **********************************************************)
  let compile_reduce payload =
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
    let impPayload = SSA_to_Imp.translate_fundef P.fnTable payload in
    let impfn =
      ImpReduceTemplate.gen_reduce_2d_capable inType impPayload threadsPerBlock  
    in
    (*
    IFDEF DEBUG THEN
      Printf.sprintf "[compile_reduce] %s\n" (Imp.fn_to_str impfn);
      flush stdout;
    ENDIF;
    *)
    let retTypes = impfn.Imp.output_types in 
    let inputSpaces = Array.map (fun t -> PtxVal.TEX) retTypes in
    let ptx, cc = 
      ImpToPtx.translate_kernel ?input_spaces:(Some inputSpaces) impfn 
    in
  
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
  let initTypes = List.map GpuVal.get_type initArgs in  
  let vecTypes = List.map GpuVal.get_type args in 
  (*
  IFDEF DEBUG THEN 
    Printf.printf "Launching Reduce kernel\n";
  ENDIF;
  *)
  let cacheKey = payload.SSA.fn_id, initTypes @ vecTypes  in 
  let {imp_source=impKernel; cc=cc; cuda_module=compiledModule} =  
    if Hashtbl.mem reduceCache cacheKey then 
      Hashtbl.find reduceCache cacheKey
    else (
      let entry =  compile_reduce payload  in 
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
  let inputArgs = ref [gpuVal] in 
  while !currInputElts > 1 do 
    let numOutputElts = safe_div !currInputElts (threadsPerBlock * 2) in
    let modulePtr = compiledModule.Cuda.module_ptr in  
    let args, outputsList =
      create_args modulePtr !currInputElts impKernel cc !inputArgs 
    in
    let gridParams = {
      CudaModule.threads_x=x_threads; threads_y=256; threads_z=1;
      grid_x=x_grid; grid_y=numOutputElts;
    }
    in
    CudaModule.launch_ptx compiledModule.Cuda.module_ptr fnName args gridParams;
    inputArgs := outputsList; 
    currInputElts := numOutputElts; 
  done; 
  let result = GpuVal.slice (List.hd !inputArgs) 0 in
  IFDEF DEBUG THEN
    print_string "\n --- REDUCE ---\n";
    Printf.printf "[GpuRuntime] REDUCE input: %s\n" (GpuVal.to_str gpuVal); 
    Printf.printf "[GpuRuntime] REDUCE output: %s\n" (GpuVal.to_str result); 
  ENDIF;
  [result]
    

  (**********************************************************
                           ALLPAIRS 
   **********************************************************)
  let compile_all_pairs payload argTypes = 
    match argTypes with 
    | [t1; t2] ->
      let threadsPerBlock = 256 in
      let impPayload = SSA_to_Imp.translate_fundef P.fnTable payload in
      let retTypes =
        List.map 
          (fun t -> DynType.VecT (DynType.VecT t))
          payload.SSA.fn_output_types 
      in 
      let impfn =
        ImpAllPairsTemplate.gen_all_pairs_2d_naive impPayload t1 t2 retTypes
      in
		  let inputSpaces =
        Array.map (fun t -> PtxVal.TEX) (Array.of_list argTypes) in
		  let kernel, cc =
        ImpToPtx.translate_kernel ~input_spaces:inputSpaces impfn in
      (*let kernel, cc = ImpToPtx.translate_kernel impfn in*)
      let allPairsPrefix = "all_pairs_kernel" in
      let name = allPairsPrefix ^ (string_of_int (ID.gen())) in
      let compiledModule =
        CudaModule.cuda_module_from_kernel_list [name, kernel] threadsPerBlock
      in
      {imp_source=impfn; cc=cc; cuda_module=compiledModule}
    | _ -> failwith "[compile_all_pairs] invalid argument types "

  let allPairsCache  = Hashtbl.create 127 

  let all_pairs 
      ~(payload : SSA.fundef) ~(payloadClosureArgs:values) ~(args:values) =
  let inputTypes = List.map GpuVal.get_type args in 
  let cacheKey = payload.SSA.fn_id, inputTypes in 
  let {imp_source=impKernel; cc=cc; cuda_module=compiledModule} = 
    if Hashtbl.mem allPairsCache cacheKey then 
      Hashtbl.find allPairsCache cacheKey
    else (
      let entry = compile_all_pairs payload inputTypes in 
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
    let elType = DynType.elt_type inputType in
    let output = 
        MemoryState.mk_gpu_vec P.memState inputType outputShape 
    in
    let inputPtr = GpuVal.get_ptr inputVec in
    let indexPtr = GpuVal.get_ptr indexVec in
    Kernels.bind_index_idxs_tex indexPtr nidxs;
    begin match elType with
    | DynType.Int32T -> begin
        Kernels.bind_index_int_vecs_tex inputPtr ninputels;
        Kernels.index_int ninputs vec_len nidxs output.vec_ptr;
        Kernels.unbind_index_int_vecs_tex ()
      end
    | DynType.Float32T -> begin
        Kernels.bind_index_float_vecs_tex inputPtr ninputels;
        Kernels.index_float ninputs vec_len nidxs output.vec_ptr;
        Kernels.unbind_index_float_vecs_tex ()
      end
    | _ -> 
        failwith $ 
          Printf.sprintf  
            "[GpuRuntime] unsupported element type for indexing (%s)"
            (DynType.to_str elType)
    end;
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
      let int32_vec_type = DynType.VecT DynType.Int32T in  
      let scanInterm = 
        MemoryState.mk_gpu_vec P.memState int32_vec_type scanShape 
      in 
      Thrust.thrust_prefix_sum_bool_to_int binPtr nelts scanInterm.vec_ptr;
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
      let output = 
        MemoryState.mk_gpu_vec P.memState int32_vec_type outputShape 
      in
      Kernels.bind_where_tex scanInterm.vec_ptr nelts;
      Kernels.where_tex nelts output.vec_ptr; 
      Kernels.unbind_where_tex ();
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
