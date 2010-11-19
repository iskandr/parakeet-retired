(* pp: -parser o pa_macro.cmo *)

open Base
open Bigarray
open HostVal
open Printf 

open ImpShapeInference
open ShapeInference

type adverb_impl =
 MemoryState.t ->  FnTable.t -> SSA.fundef ->
    GpuVal.gpu_val list -> GpuVal.gpu_val list -> 
      DynType.t list -> GpuVal.gpu_val list

type simple_array_op_impl =
  MemoryState.t -> FnTable.t ->
    GpuVal.gpu_val list -> DynType.t list -> GpuVal.gpu_val list

exception InvalidGpuArgs

let sizeof ty shape =
  DynType.sizeof (DynType.elt_type ty) * Shape.nelts shape

type code_cache_entry = {
  imp_source : Imp.fn;
  cc : PtxCallingConventions.calling_conventions;
  cuda_module : Cuda.cuda_module
}
   
type code_cache = (ID.t * DynType.t list, code_cache_entry) Hashtbl.t 

(* any given gpu argument might either get placed into the array 
   of explicit args or it might become an implicit argument via a
   global texture
*) 
let create_input_args modulePtr inputVal = function
  | PtxCallingConventions.ScalarInput ->
      [LibPQ.GpuScalarArg(GpuVal.get_scalar inputVal)]
  | PtxCallingConventions.GlobalInput ->
        [LibPQ.GpuArrayArg((GpuVal.get_ptr inputVal),
                           (GpuVal.get_nbytes inputVal));
         LibPQ.GpuArrayArg((GpuVal.get_shape_ptr inputVal),
                           (GpuVal.get_shape_nbytes inputVal))]
  | PtxCallingConventions.TextureInput (texName, geom) ->
    let texRef = Cuda.cuda_module_get_tex_ref modulePtr texName in
    let inputShape = GpuVal.get_shape inputVal in
    let inputPtr = GpuVal.get_ptr inputVal in
    let channelFormat = 
      Cuda.infer_channel_format 
        (DynType.elt_type $ GpuVal.get_type inputVal)
    in
    begin match geom with
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
        (Shape.get inputShape 0)
        (Shape.get inputShape 1)
        channelFormat  
    | _ -> failwith "3D textures not yet implemented"
    end;
    [LibPQ.GpuArrayArg((GpuVal.get_shape_ptr inputVal),
                       (GpuVal.get_shape_nbytes inputVal))] 
  
let create_args
      (modulePtr : Cuda.CuModulePtr.t)
      (impfn : Imp.fn)
      (cc : PtxCallingConventions.calling_conventions)
      (inputs: GpuVal.gpu_val list) =
  let inputShapes = List.map GpuVal.get_shape inputs in 
  let shapeEnv = ImpShapeInference.infer_shapes impfn inputShapes in
  let initEnv =
    Array.fold_left2
      (fun env id gpuVal ->
        let dataLocation =
          ID.Map.find id cc.PtxCallingConventions.data_locations in
        let gpuArgs = create_input_args modulePtr gpuVal dataLocation in
        ID.Map.add id gpuArgs env)
      ID.Map.empty
      impfn.Imp.input_ids
      (Array.of_list inputs)
  in
  let outputMap = DynArray.create() in
  let valueEnv = Array.fold_left
    (fun env id ->
      assert (PMap.mem id impfn.Imp.tenv);
      (* don't yet handle scalar outputs *)
      let ty = PMap.find id impfn.Imp.tenv in
      assert (DynType.is_vec ty);
      assert (ID.Map.mem id shapeEnv);
      let shape = ID.Map.find id shapeEnv in
      assert (Shape.rank shape > 0);
      let outputVal = GpuVal.mk_gpu_vec ty shape in
      DynArray.add outputMap outputVal;
      ID.Map.add
        id
        [LibPQ.GpuArrayArg((GpuVal.get_ptr outputVal),
                           (GpuVal.get_nbytes outputVal));
         LibPQ.GpuArrayArg((GpuVal.get_shape_ptr outputVal),
                           (GpuVal.get_shape_nbytes outputVal))]
        env
    )
    initEnv
    impfn.Imp.output_ids
  in
  let paramsArray = DynArray.create() in
  Array.iter
    (fun id ->
      let args = ID.Map.find id valueEnv in
      List.iter (DynArray.add paramsArray) args)
    cc.PtxCallingConventions.param_order
  ;
  (DynArray.to_array paramsArray), (DynArray.to_list outputMap)


(** MAP **)
let map_id_gen = mk_gen()
let compile_map globalFunctions payload closureTypes argTypes retTypes =
  let mapThreadsPerBlock = 256 in
  (* converting payload to Imp *) 
  let impPayload = SSA_to_Imp.translate_fundef globalFunctions payload in
  (* generating Imp kernel w/ embedded payload *)
  let impfn =
    ImpMapTemplate.gen_map
      impPayload
      mapThreadsPerBlock
      (Array.of_list closureTypes)
      (Array.of_list argTypes)
      (Array.of_list retTypes)
  in
  let inputSpaces = Array.map (fun t -> PtxVal.TEX) (Array.of_list argTypes) in
  let kernel, cc = ImpToPtx.translate_kernel
                     ?input_spaces:(Some inputSpaces) impfn in
  (*let kernel, cc = ImpToPtx.translate_kernel impfn in*)
  let kernelName = "map_kernel" ^ (string_of_int (map_id_gen())) in
  let cudaModule =
    LibPQ.cuda_module_from_kernel_list [kernelName, kernel] mapThreadsPerBlock
  in
  {imp_source=impfn; cc=cc; cuda_module=cudaModule}

let mapCache : code_cache = Hashtbl.create 127
 
let run_map memState globalFunctions payload closureVals gpuVals outputTypes =
  let closureTypes = List.map GpuVal.get_type closureVals in 
  let inputTypes = List.map GpuVal.get_type gpuVals in
  let cacheKey = (payload.SSA.fn_id, closureTypes @ inputTypes) in  
  let {imp_source=impKernel; cc=cc; cuda_module=cudaModule} = 
    if Hashtbl.mem mapCache cacheKey then 
      Hashtbl.find mapCache cacheKey
    else (
      let entry = 
        compile_map 
          globalFunctions 
          payload 
          closureTypes 
          inputTypes 
          outputTypes 
      in
      Hashtbl.add mapCache cacheKey entry; 
      entry
    )  
  in
  let paramsArray, outputVals =
    create_args cudaModule.Cuda.module_ptr impKernel cc (closureVals @ gpuVals)
  in  
  (* create one CUDA thread per every input element *) 
  assert (List.length cudaModule.Cuda.kernel_names = 1); 
  let fnName = List.hd cudaModule.Cuda.kernel_names in
  let maxShape = 
    match Shape.max_shape_list (List.map GpuVal.get_shape gpuVals) with 
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
  LibPQ.launch_ptx cudaModule.Cuda.module_ptr fnName paramsArray gridParams;
  outputVals

(** REDUCE **)
let compile_reduce globalFunctions payload retTypes =
  let redThreadsPerBlock = 256 in
  let impPayload = SSA_to_Imp.translate_fundef globalFunctions payload in
  let impfn =
    ImpReduceTemplate.gen_reduce_2d_capable
      impPayload redThreadsPerBlock retTypes 
  in
  IFDEF DEBUG THEN
	  Printf.sprintf "[compile_reduce] %s\n" (Imp.fn_to_str impfn);
	  flush stdout;
  ENDIF;
  
  let inputSpaces = Array.map (fun t -> PtxVal.TEX) (Array.of_list retTypes) in
  let ptx, cc = ImpToPtx.translate_kernel
                     ?input_spaces:(Some inputSpaces) impfn in
  (*let ptx, cc = ImpToPtx.translate_kernel impfn in*)
  let reducePrefix = "reduce_kernel" in
  let name = reducePrefix ^ (string_of_int (ID.gen())) in
  let cudaModule = 
    LibPQ.cuda_module_from_kernel_list [name,ptx] redThreadsPerBlock
  in 
  {imp_source=impfn; cc=cc; cuda_module=cudaModule} 

let reduceCache : code_cache  = Hashtbl.create 127 

let run_reduce 
      (memState : MemoryState.t)
      (globalFunctions : FnTable.t)
      (payload : SSA.fundef)
      (gpuVals : GpuVal.gpu_val list)
      (outputTypes : DynType.t list) = 
  let inputTypes = List.map GpuVal.get_type gpuVals in 
  IFDEF DEBUG THEN 
    Printf.printf "Launching Reduce kernel\n";
  ENDIF;
  let cacheKey = payload.SSA.fn_id, inputTypes in 
  let {imp_source=impKernel; cc=cc; cuda_module=compiledModule} =  
    if Hashtbl.mem reduceCache cacheKey then 
      Hashtbl.find reduceCache cacheKey
    else (
      let entry =  
        compile_reduce globalFunctions payload outputTypes 
      in 
      Hashtbl.add reduceCache cacheKey entry; 
      entry
    )
  in 
  let threadsPerBlock = compiledModule.Cuda.threads_per_block in
  assert (List.length outputTypes = 1); 
  let outputType = List.hd outputTypes in
  match compiledModule.Cuda.kernel_names, gpuVals with
    (* WAYS THIS IS CURRENTLY WRONG: 
       - we are ignoring the initial value
       - we are only allowing reductions over a single array
       - in the 2D case, we only support embedded maps
    *)
    | [fnName], _ :: [gpuVal] ->
      let inShape = GpuVal.get_shape gpuVal in
      let numInputElts = Shape.get inShape 0 in
      let x_threads = 1 in
      let x_grid =
        if Shape.rank inShape = 1 then 1
        else (Shape.get inShape 1) / x_threads
      in
      IFDEF DEBUG THEN Printf.printf "Launching x grid: %d\n" x_grid; ENDIF;
      let rec aux inputArgs curNumElts =
        if curNumElts > 1 then (
          let numOutputElts = safe_div curNumElts (threadsPerBlock * 2) in
          let args, outputsList =
            create_args compiledModule.Cuda.module_ptr impKernel cc inputArgs in
		      let gridParams = {
		        LibPQ.threads_x=x_threads; threads_y=256; threads_z=1;
		        grid_x=x_grid; grid_y=numOutputElts;
		      }
		      in
          LibPQ.launch_ptx
            compiledModule.Cuda.module_ptr fnName args gridParams;
          if curNumElts < numInputElts then GpuVal.free (List.hd inputArgs);
          aux outputsList numOutputElts
          )
        else (
          let result = GpuVal.get_slice (List.hd inputArgs) 0 in 
          IFDEF DEBUG THEN 
            Printf.printf "Final reduction result of shape %s, type %s\n"
              (Shape.to_str (GpuVal.get_shape result))
              (DynType.to_str (GpuVal.get_type result))
            ; 
          ENDIF;
          [result]
        )
      in
      aux [gpuVal] numInputElts
    | _ -> failwith "expect one reduce kernel"

(** ALLPAIRS **)
let compile_all_pairs globalFunctions payload argTypes retTypes =
  match argTypes with 
    | [t1; t2] ->
      let threadsPerBlock = 256 in
      let impPayload = SSA_to_Imp.translate_fundef globalFunctions payload in
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
        LibPQ.cuda_module_from_kernel_list [name, kernel] threadsPerBlock
      in
      {imp_source=impfn; cc=cc; cuda_module=compiledModule}
    | _ -> failwith "[compile_all_pairs] invalid argument types "

let allPairsCache  = Hashtbl.create 127 

let run_all_pairs
      (memState : MemoryState.t)
      (globalFunctions : FnTable.t)
      (payload : SSA.fundef)
      (gpuVals : GpuVal.gpu_val list)
      (outputTypes : DynType.t list) =
  let inputTypes = List.map GpuVal.get_type gpuVals in 
  let cacheKey = payload.SSA.fn_id, inputTypes in 
  let {imp_source=impKernel; cc=cc; cuda_module=compiledModule} = 
    if Hashtbl.mem allPairsCache cacheKey then 
      Hashtbl.find allPairsCache cacheKey
    else (
      let entry = 
        compile_all_pairs globalFunctions payload inputTypes outputTypes 
      in 
      Hashtbl.add allPairsCache cacheKey entry;
      entry
    )
  in
  match compiledModule.Cuda.kernel_names, gpuVals with
    | _, [] | _, [_] | _, _::_::_::_ ->  
        failwith "[run_all_pairs] wrong number of arguments"
    | [], _ | _::_::_, _ ->
        failwith "[run_all_pairs] wrong number of functions" 
    | [fnName], [xGpu;yGpu] ->
      let xShape, yShape = GpuVal.get_shape xGpu, GpuVal.get_shape yGpu in
      let nx = Shape.get xShape 0 in
      let ny = Shape.get yShape 0 in
      let paramsArray, outputVals =
        create_args compiledModule.Cuda.module_ptr impKernel cc gpuVals in 
      let gridParams = {
          LibPQ.threads_x=16; threads_y=16; threads_z=1;
          grid_x=safe_div nx 16; grid_y=safe_div ny 16;
      }
      in
      LibPQ.launch_ptx
        compiledModule.Cuda.module_ptr fnName paramsArray gridParams;
      outputVals

(** INDEX **)
let run_index
      (memState : MemoryState.t)
      (inputVec : GpuVal.gpu_val) 
      (indexVec : GpuVal.gpu_val) =
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
  let elType = DynType.elt_type (GpuVal.get_type inputVec) in
  let output = GpuVal.mk_gpu_vec (GpuVal.get_type inputVec) outputShape in
  let inputPtr = GpuVal.get_ptr inputVec in
  let indexPtr = GpuVal.get_ptr indexVec in
  let outputPtr = GpuVal.get_ptr output in
  Kernels.bind_index_idxs_tex indexPtr nidxs;
  begin match elType with
    | DynType.Int32T -> begin
        Kernels.bind_index_int_vecs_tex inputPtr ninputels;
        Kernels.index_int ninputs vec_len nidxs outputPtr;
        Kernels.unbind_index_int_vecs_tex ()
      end
    | DynType.Float32T -> begin
        Kernels.bind_index_float_vecs_tex inputPtr ninputels;
        Kernels.index_float ninputs vec_len nidxs outputPtr;
        Kernels.unbind_index_float_vecs_tex ()
      end
    | _ -> failwith "[run_index] unsupported type for indexing"
  end;
  [output]

(** WHERE **)
let run_where
      (memState : MemoryState.t)
      (binVec   : GpuVal.gpu_val) =
  let nelts = GpuVal.nelts binVec in
  let scanShape = GpuVal.get_shape binVec in
  let scanInterm = GpuVal.mk_gpu_vec (DynType.VecT DynType.Int32T) scanShape in
  let binPtr = GpuVal.get_ptr binVec in
  let scanPtr = GpuVal.get_ptr scanInterm in
  Thrust.thrust_prefix_sum_bool_to_int binPtr nelts scanPtr;
  let numIdxs = Cuda.cuda_get_gpu_int_vec_element scanPtr (nelts - 1) in
  let outputShape = Shape.create 1 in
  Shape.set outputShape 0 numIdxs;
  let output = GpuVal.mk_gpu_vec (DynType.VecT DynType.Int32T) outputShape in
  Kernels.bind_where_tex scanPtr nelts;
  Kernels.where_tex nelts (GpuVal.get_ptr output);
  Kernels.unbind_where_tex ();
  [output]

(*
let init () =
  (* initialize GPU contexts and device info *)
  LibPQ.cuda_init();
  HardwareInfo.hw_init()
*)
  
let shutdown () =
  ()
  (*
  for i = 0 to DynArray.length HardwareInfo.device_contexts - 1 do 
    Cuda.cuda_ctx_destroy (DynArray.get HardwareInfo.device_contexts i)
  done
  *)
 
let implements_array_op = function 
  | Prim.Reduce | Prim.Map | Prim.AllPairs | Prim.Index | Prim.Where -> true
  | _ -> false 
 
let eval_array_op
      (memState : MemoryState.t)
      (fnTable : FnTable.t)
      (op : Prim.array_op)
      (args : InterpVal.t list)
      (outputTypes : DynType.t list)
      : InterpVal.t list =
  IFDEF DEBUG THEN 
     Printf.printf "In eval array op\n";
  ENDIF; 
  let gpuVals = match op, args  with  
  | Prim.Map, (InterpVal.Closure(fnId, closureArgs))::dataArgs ->
      let fundef = FnTable.find fnId fnTable in
      let closureVals = List.map (MemoryState.get_gpu memState) closureArgs in 
      let dataVals = List.map (MemoryState.get_gpu memState) dataArgs in
      run_map memState fnTable fundef closureVals dataVals outputTypes 
      
  | Prim.Map, _ -> 
      failwith "[GpuRuntime->eval_array_op] closures not yet supported for Map"

  | Prim.Reduce, (InterpVal.Closure(fnId, []))::dataArgs ->

      let fundef = FnTable.find fnId fnTable in
      (* the current reduce kernel works either for 1d data or 
         for maps over 2d data 
      *) 
      let fundef2 = match SSA.extract_nested_map_fn_id fundef with 
        | Some nestedFnId -> FnTable.find nestedFnId fnTable
        | None -> fundef 
      in  
      let gpuVals = List.map (MemoryState.get_gpu memState) dataArgs in
      run_reduce memState fnTable fundef2 gpuVals outputTypes
  
  | Prim.Reduce, _ -> 
      failwith 
        "[GpuRuntime->eval_array_op] closures not yet supported for Reduce"
        
  | Prim.AllPairs, (InterpVal.Closure(fnId, []))::dataArgs ->
      let fundef = FnTable.find fnId fnTable in
      let gpuVals = List.map (MemoryState.get_gpu memState) dataArgs in
      run_all_pairs  memState fnTable fundef gpuVals outputTypes  
  | Prim.AllPairs, _ ->
      failwith 
        "[GpuRuntime->eval_array_op] closures not yet supported for AllPairs"

  | Prim.Index, [inputVec;indexVec] ->
      let gpuInputVec = MemoryState.get_gpu memState inputVec in
      let gpuIndexVec = MemoryState.get_gpu memState indexVec in
      run_index memState gpuInputVec gpuIndexVec
  | Prim.Index, _ ->
      failwith "[GpuRuntime->eval_array_op] index requires 2 vec inputs"

  | Prim.Where, [binVec] ->
	    let gpuBinVec = MemoryState.get_gpu memState binVec in
      run_where memState gpuBinVec
  | Prim.Where, _ ->
      failwith "[GpuRuntime->eval_array_op] where requires single binary vec"

  | _ -> failwith $ Printf.sprintf 
    "Array operator '%s' not yet implemented on GPU"
    (Prim.array_op_to_str op)
  in 
  List.map (MemoryState.add_gpu memState) gpuVals
