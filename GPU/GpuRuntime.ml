open Base
open Bigarray
open HostVal
open Printf 

open ShapeInference 
open ImpShapeInference 

type adverb_impl = 
 MemoryState.mem_state ->  FnTable.t -> SSA.fundef -> 
    GpuVal.gpu_val list -> DynType.t list -> GpuVal.gpu_val list

type simple_array_op_impl =
  MemoryState.mem_state -> FnTable.t -> 
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
let create_input_arg modulePtr argsDynArray inputVal = function
  | PtxCallingConventions.ScalarInput 
  | PtxCallingConventions.GlobalInput ->  DynArray.add argsDynArray inputVal
  | PtxCallingConventions.TextureInput (texName, geom) ->  
    let texRef = Cuda.cuda_module_get_tex_ref modulePtr texName in
    let inputShape = GpuVal.get_shape inputVal in
    let inputPtr = GpuVal.get_ptr inputVal in
    let channelFormat = 
      Cuda.infer_channel_format 
        (DynType.elt_type $ GpuVal.get_type inputVal)
    in  
    begin match geom with 
    | Ptx.Tex2D ->
      assert (Shape.rank inputShape = 2); 
      Cuda.cuda_bind_texture_2d_std_channel
        texRef
        inputPtr
        (Shape.get inputShape 0)
        (Shape.get inputShape 1)
        channelFormat  
    | _ -> failwith "non 2D textures not yet implemented"
    end 
    
      
let create_gpu_arg_env 
    ~(input_ids : ID.t list)
    ~(input_vals : GpuVal.gpu_val list)
    ~(local_ids : ID.t list)
    ~(output_ids : ID.t list) 
    ~(shape_env : Shape.t ID.Map.t)
    ~(type_env : (ID.t, DynType.t) PMap.t) =
  (* for now ignore calling_conventions.data_locations *)
  let initEnv =  
    List.fold_left2 
      (fun env id gpuVal  -> ID.Map.add id gpuVal env)
      ID.Map.empty  
      input_ids
      input_vals
  in 
  List.fold_left  
    (fun env id -> 
      if not $ ID.Map.mem id env then (
        let ty = PMap.find id type_env in
        if not $ DynType.is_scalar ty then (
          let shape = ID.Map.find id shape_env in
          (* don't yet handle scalar outputs *) 
          assert (Shape.rank shape > 0);
          ID.Map.add id (GpuVal.mk_gpu_vec ty shape) env
        )
        else env 
      )
      else env   
    )
    initEnv
    (local_ids @ output_ids)
  
  
let create_args
      (impfn : Imp.fn)
      (cc : PtxCallingConventions.calling_conventions)
      (inputs: GpuVal.gpu_val list) =
  let inputShapes = List.map GpuVal.get_shape inputs in 
  let shapeEnv = ImpShapeInference.infer_shapes impfn inputShapes in
  let outputIds = (Array.to_list impfn.Imp.output_ids) in 
  let valueEnv = 
    create_gpu_arg_env
      ~input_ids:(Array.to_list impfn.Imp.input_ids)
      ~input_vals:inputs
      ~local_ids:(Array.to_list impfn.Imp.local_ids)
      ~output_ids:outputIds
      ~shape_env:shapeEnv
      ~type_env:impfn.Imp.tenv
  in 
  let paramsArray = 
    Array.map 
      (fun id -> ID.Map.find id valueEnv) 
      cc.PtxCallingConventions.param_order
  in
  let outputsList = List.map (fun id -> ID.Map.find id valueEnv) outputIds in
  paramsArray, outputsList 


(** MAP **)
let map_id_gen = mk_gen()
let compile_map globalFunctions payload argTypes retTypes =
  let mapThreadsPerBlock = 128 in
  (* converting payload to Imp *) 
  let impPayload = SSA_to_Imp.translate_fundef globalFunctions payload in
  (* generating Imp kernel w/ embedded payload *)
  let impfn = 
    ImpMapTemplate.gen_map 
      impPayload 
      mapThreadsPerBlock 
      (Array.of_list argTypes) 
      (Array.of_list retTypes) 
  in   
  let kernel, cc = ImpToPtx.translate_kernel impfn in
  let kernelName = "map_kernel" ^ (string_of_int (map_id_gen())) in
  let cudaModule = 
    LibPQ.cuda_module_from_kernel_list [kernelName, kernel] mapThreadsPerBlock
  in    
  {imp_source=impfn; cc=cc; cuda_module=cudaModule} 

let mapCache : code_cache = Hashtbl.create 127
 
let run_map memState globalFunctions payload gpuVals outputTypes =
  let inputTypes = List.map GpuVal.get_type gpuVals in  
  let cacheKey = (payload.SSA.fn_id, inputTypes) in  
  let {imp_source=impKernel; cc=cc; cuda_module=cudaModule} = 
    if Hashtbl.mem mapCache cacheKey then 
      Hashtbl.find mapCache cacheKey
    else (
      let entry = 
        compile_map globalFunctions payload inputTypes outputTypes 
      in
      Hashtbl.add mapCache cacheKey entry; 
      entry
    )  
  in      
  let paramsArray, outputVals = create_args impKernel cc gpuVals in  
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
    ImpReduceTemplate.gen_reduce_2d_capable impPayload redThreadsPerBlock retTypes 
  in
  IFDEF DEBUG THEN
	  Printf.sprintf "[compile_reduce] %s\n" (Imp.fn_to_str impfn);
	  flush stdout;
  ENDIF;
  let ptx, cc = ImpToPtx.translate_kernel impfn in
  let reducePrefix = "reduce_kernel" in
  let name = reducePrefix ^ (string_of_int (ID.gen())) in
  let cudaModule = 
    LibPQ.cuda_module_from_kernel_list [name,ptx] redThreadsPerBlock
  in 
  {imp_source=impfn; cc=cc; cuda_module=cudaModule} 

let reduceCache : code_cache  = Hashtbl.create 127 

let run_reduce 
      (memState : MemoryState.mem_state)
      (globalFunctions : FnTable.t)
      (payload : SSA.fundef)
      (gpuVals : GpuVal.gpu_val list)
      (outputTypes : DynType.t list) = 
  let inputTypes = List.map GpuVal.get_type gpuVals in 
  let cacheKey = payload.SSA.fn_id, inputTypes in 
  let {imp_source=impKernel; cc=_; cuda_module=compiledModule} =  
    if Hashtbl.mem reduceCache cacheKey then Hashtbl.find reduceCache cacheKey
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
       - we only deal with 1D arrays
       - only scalar outputs are allowed  
    *) 
    | [fnName], _ :: [gpuVal] ->
      let numInputElts = Shape.nelts (GpuVal.get_shape gpuVal) in
      let rec aux inputArg curNumElts =
        if curNumElts > 1 then (
          let numOutputElts = safe_div curNumElts (threadsPerBlock * 2) in
          let newShape = Shape.of_list [numOutputElts] in
          let newOut = GpuVal.mk_gpu_vec (DynType.VecT outputType) newShape in 
          let args = Array.of_list ([inputArg; newOut]) in
          (* TODO: Only will work for width 1 *)
          let gridParams = {
            LibPQ.threads_x=1; threads_y=256; threads_z=1;
            grid_x=1; grid_y=numOutputElts;
          }
          in
          (*
          let gridParams = match
	            HardwareInfo.get_grid_params
	              ~device:0
	              ~block_size:threadsPerBlock
	              (safe_div curNumElts 2)
	            with
	            | Some gridParams -> gridParams
	            | None -> failwith
	                (sprintf "Unable to get launch params for %d elts" curNumElts)
	        in
          *)
          Printf.printf "Launching with %d inputs, %d outputs\n"
            curNumElts numOutputElts;
          flush stdout;
          LibPQ.launch_ptx
            compiledModule.Cuda.module_ptr fnName args gridParams;
          if curNumElts < numInputElts then GpuVal.free args.(0);
          aux newOut numOutputElts
          )
        else
          inputArg
      in
      let ret = aux gpuVal numInputElts in
      [ret]
    | _ -> failwith "expect one reduce kernel"

(** ALLPAIRS **)
let compile_all_pairs globalFunctions payload argTypes retTypes =
  match argTypes with 
    | [t1; t2] ->  
      
      let threadsPerBlock = 128 in
      let impPayload = SSA_to_Imp.translate_fundef globalFunctions payload in
      let impfn =
        ImpAllPairsTemplate.gen_all_pairs_2d_naive impPayload t1 t2 retTypes
      in
      let kernel, cc = ImpToPtx.translate_kernel impfn in
      let allPairsPrefix = "all_pairs_kernel" in 
      let name = allPairsPrefix ^ (string_of_int (ID.gen())) in
      let compiledModule = 
        LibPQ.cuda_module_from_kernel_list [name, kernel] threadsPerBlock 
      in   
      {imp_source=impfn; cc=cc; cuda_module=compiledModule} 
    | _ -> failwith "[compile_all_pairs] invalid argument types "

let allPairsCache  = Hashtbl.create 127 

let run_all_pairs
      (memState : MemoryState.mem_state)
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
      let paramsArray, outputVals = create_args impKernel cc gpuVals in 
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
      (memState : MemoryState.mem_state)
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
      (memState : MemoryState.mem_state)
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
  
let eval_array_op
      (memState : MemoryState.mem_state)
      (functions : FnTable.t)
      (op : Prim.array_op)
      (args : InterpVal.t list)
      (outputTypes : DynType.t list)
      : GpuVal.gpu_val list =
  IFDEF DEBUG THEN 
     Printf.printf "In eval array op\n";
  ENDIF; 
  match op, args  with  
  | Prim.Map, (InterpVal.Closure(fnId, []))::dataArgs ->
      let fundef = FnTable.find fnId functions in
      let gpuVals = List.map (MemoryState.get_gpu memState) dataArgs in 
      run_map memState functions   fundef gpuVals outputTypes 
      
  | Prim.Map, _ -> 
      failwith "[GpuRuntime->eval_array_op] closures not yet supported for Map"

  | Prim.Reduce, (InterpVal.Closure(fnId, []))::dataArgs ->
      let fundef = FnTable.find fnId functions in
      let gpuVals = List.map (MemoryState.get_gpu memState) dataArgs in
      run_reduce memState functions fundef gpuVals outputTypes
  
  | Prim.Reduce, _ -> 
      failwith 
        "[GpuRuntime->eval_array_op] closures not yet supported for Reduce"
        
  | Prim.AllPairs, (InterpVal.Closure(fnId, []))::dataArgs ->
      let fundef = FnTable.find fnId functions in
      let gpuVals = List.map (MemoryState.get_gpu memState) dataArgs in
      run_all_pairs  memState functions fundef gpuVals outputTypes  
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
