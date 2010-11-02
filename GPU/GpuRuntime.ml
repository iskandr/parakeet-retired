open Base
open Bigarray
open HostVal
open Printf 

open ShapeInference 

exception InvalidGpuArgs 

let sizeof ty shape =
  DynType.sizeof (DynType.elt_type ty) * Shape.nelts shape  


type adverb_cache = (ID.t * DynType.t list, Cuda.cuda_module) Hashtbl.t 

let mk_cuda_module 
    (kernelList : (string * Ptx.kernel) list) 
    threadsPerBlock =
 
  let ptxModule = Ptx.module_from_named_kernels kernelList in  
  let ptxStr = Ptx.ptx_module_to_str ptxModule in
  print_string ptxStr;
  flush stdout; 
  let modulePtr = LibPQ.compile_module ptxStr threadsPerBlock in
  (* take an input space and change it from referring to 
     kernel-local symids to module-level names 
  *) 
  let stringify_textured_input symbols = function 
    | Ptx.GlobalInput -> Ptx.GlobalInput 
    | Ptx.TextureInput (symid, geom) -> 
        let texName= Hashtbl.find symbols symid in 
        Ptx.TextureInput (texName, geom) 
  in 
  let get_calling_conventions kernel : string Ptx.calling_conventions  =
    let callingConventions : Ptx.symid Ptx.calling_conventions =
       kernel.Ptx.calling_conventions
    in   
    { 
      Ptx.input_spaces = 
        Array.map 
          (stringify_textured_input kernel.Ptx.symbols)
          callingConventions.Ptx.input_spaces  
    }
     
  in 
  { 
    Cuda.module_ptr = modulePtr;
    kernels =  
      List.map 
        (fun (name,kernel)-> name, get_calling_conventions kernel) 
        kernelList;
    threads_per_block =  threadsPerBlock
  } 


(* any given gpu argument might either get placed into the array 
   of explicit args or it might become an implicit argument via a
   global texture
*) 
let create_input_arg modulePtr argsDynArray inputVal = function 
  | Ptx.GlobalInput ->  DynArray.add argsDynArray inputVal
  | Ptx.TextureInput (texName, geom) ->  
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
    
      
let create_gpu_args 
      (modulePtr : Cuda.CuModulePtr.t) 
      ~(inputs : GpuVal.gpu_val list)
      ~(input_spaces : string Ptx.input_space array)  
      ~( outputs : GpuVal.gpu_val list)
      ~( preallocs : GpuVal.gpu_val list)  =
  let argsArray : GpuVal.gpu_val DynArray.t = DynArray.create () in 
  List.iter2 
    (create_input_arg modulePtr argsArray)
    inputs
    (Array.to_list input_spaces) 
  ;
  List.iter (DynArray.add argsArray) outputs;
  List.iter (DynArray.add argsArray) preallocs; 
  DynArray.to_array argsArray  
  
let compile_map globalFunctions payload argTypes retTypes =
   
  let mapThreadsPerBlock = 128 in
  (* set of inputs, outputs and local temporaries which require heap 
     allocation 
  *) 
  let allocSet = AllocationAnalysis.infer_fundef payload in  
  (* converting payload to Imp *) 
  let impPayload = SSA_to_Imp.translate_fundef globalFunctions payload in
  (* generating Imp kernel w/ embedded payload *)
  let impfn = 
    ImpGenMap.gen_map 
      impPayload 
      mapThreadsPerBlock 
      (Array.of_list argTypes) 
      (Array.of_list retTypes) 
  in 
  let kernel = ImpToPtx.translate_kernel impfn in
  let mapPrefix = "map_kernel" in 
  let name = mapPrefix ^ (string_of_int (ID.gen())) in
  mk_cuda_module [name, kernel] mapThreadsPerBlock

let mapCache : adverb_cache = Hashtbl.create 127
 
let run_map globalFunctions payload inputTypes outputTypes memState dynVals =
 
  let cacheKey = (payload.SSA.fn_id, inputTypes) in  
  let compiledModule = 
    if Hashtbl.mem mapCache cacheKey then 
      Hashtbl.find mapCache cacheKey
    else (
      let m = compile_map globalFunctions payload inputTypes outputTypes in
      Hashtbl.add mapCache cacheKey m; 
      m
    )  
  in      
  assert (List.length compiledModule.Cuda.kernels = 1); 
  let fnName, callingConventions = List.hd compiledModule.Cuda.kernels in
   (* for now just transfer everything to the GPU-- we can try to make this
     adaptive later 
  *)
  let gpuVals = List.map (MemoryState.get_gpu memState) dynVals in
  let inputShapes = List.map GpuVal.get_shape gpuVals in
  let outputShapes, shapeEnv = 
    ShapeInference.infer_map globalFunctions payload inputShapes 
  in 
  let outputVals =
    List.map2 
      (fun shape ty ->  GpuVal.mk_gpu_vec ty shape) 
      outputShapes 
      outputTypes
  in
  (* create one CUDA thread per every input element *) 
  let maxShape = match Shape.max_shape_list inputShapes with 
    | Some maxShape -> maxShape  
    | None -> assert false
  in 
  let outputElts = Shape.nelts maxShape in
  let gridParams = 
    match HardwareInfo.get_grid_params ~device:0 
      ~block_size:compiledModule.Cuda.threads_per_block outputElts
    with
      | Some gridParams -> gridParams
      | None ->
        failwith (sprintf "Unable to get launch params for %d elts" outputElts)
  in
  let args = 
    create_gpu_args 

      compiledModule.Cuda.module_ptr
      ~inputs:gpuVals 
      ~input_spaces:callingConventions.Ptx.input_spaces  
      ~outputs:outputVals
      ~preallocs:[]
  in  
  LibPQ.launch_ptx compiledModule.Cuda.module_ptr fnName args gridParams; 
  outputVals



let compile_reduce globalFunctions payload retTypes =
  let redThreadsPerBlock = 128 in
  let impPayload = SSA_to_Imp.translate_fundef globalFunctions payload in
  let impfn = ImpGenReduce.gen_reduce impPayload redThreadsPerBlock retTypes in
  debug (Printf.sprintf "[compile_reduce] %s\n" (Imp.fn_to_str impfn));
  let ptx = ImpToPtx.translate_kernel impfn in
  let reducePrefix = "reduce_kernel" in 
  let name = reducePrefix ^ (string_of_int (ID.gen())) in
  mk_cuda_module [name,ptx] redThreadsPerBlock


let reduceCache : adverb_cache  = Hashtbl.create 127 

let run_reduce globalFunctions payload inputTypes outputTypes memState dynVals =
  let gpuVals = List.map (MemoryState.get_gpu memState) dynVals in
  let cacheKey = payload.SSA.fn_id, inputTypes in 
  let compiledModule = 
    if Hashtbl.mem reduceCache cacheKey then Hashtbl.find reduceCache cacheKey
    else (
      let m = compile_reduce globalFunctions payload outputTypes in 
      Hashtbl.add reduceCache cacheKey m; 
      m
    )
  in 
  let threadsPerBlock = compiledModule.Cuda.threads_per_block in
  assert (List.length outputTypes = 1); 
  let outputType = List.hd outputTypes in 
  match compiledModule.Cuda.kernels, gpuVals with
    (* WAYS THIS IS CURRENTLY WRONG: 
       - we are ignoring the initial value
       - we are only allowing reductions over a single array
       - we only deal with 1D arrays
       - only scalar outputs are allowed  
    *) 
    | [fnName, _], _ :: [gpuVal] ->
      let numInputElts = Shape.nelts (GpuVal.get_shape gpuVal) in
      let rec aux inputArg curNumElts =
        if curNumElts > 1 then (
          let numOutputElts = safe_div curNumElts (threadsPerBlock * 2) in
          let newShape = Shape.of_list [numOutputElts] in
          let newOut = GpuVal.mk_gpu_vec (DynType.VecT outputType) newShape in 
          let args = Array.of_list ([inputArg; newOut]) in
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
    | _ -> failwith "expect one map kernel"


let compile_all_pairs globalFunctions payload argTypes retTypes =
  match argTypes with 
    | [t1; t2] ->  
      
      let threadsPerBlock = 128 in
      let impPayload = SSA_to_Imp.translate_fundef globalFunctions payload in
      let impfn =
        ImpGenAllPairs.gen_all_pairs_2d_naive impPayload t1 t2 retTypes
      in
      let kernel = ImpToPtx.translate_kernel impfn in
      let allPairsPrefix = "all_pairs_kernel" in 
      let name = allPairsPrefix ^ (string_of_int (ID.gen())) in  
      mk_cuda_module [name, kernel] threadsPerBlock 
      
    | _ -> failwith "[compile_all_pairs] invalid argument types "

let allPairsCache  = Hashtbl.create 127 
 
let run_all_pairs globalFunctions payload inputTypes outputTypes memState dynVals =
  let cacheKey = payload.SSA.fn_id, inputTypes in 
  let compiledModule = 
    if Hashtbl.mem allPairsCache cacheKey then 
      Hashtbl.find allPairsCache cacheKey
    else (
      let m = 
        compile_all_pairs globalFunctions payload inputTypes outputTypes 
      in 
      Hashtbl.add allPairsCache cacheKey m;
      m
    )
  in
  match compiledModule.Cuda.kernels, dynVals with
    | _, [] 
    | _, [_] | _, _::_::_::_ ->  
        failwith "[run_all_pairs] wrong number of arguments"
    | [], _ 
    | _::_::_, _ ->
        failwith "[run_all_pairs] wrong number of functions" 
    | [fnName, _], [x;y] ->
      (* until we have proper shape inference, we can only deal with functions 
        that return scalars 
      *) 
      let fnReturnTypes = DynType.fn_output_types payload.SSA.fn_type in 
      assert (List.for_all DynType.is_scalar fnReturnTypes);  
      let xGpu = MemoryState.get_gpu memState x  in
      let yGpu = MemoryState.get_gpu memState y in
      let xShape, yShape = GpuVal.get_shape xGpu, GpuVal.get_shape yGpu in
      (* since we assume that the nested function returns scalars, 
         the result will always be 2D
      *)
      let outputShape = Shape.create 2 in
      let nx = Shape.get xShape 0 in
      let ny = Shape.get yShape 0 in  
      Shape.set outputShape 0 nx; 
      Shape.set outputShape 1 ny;
      let outputVals =
        List.map
          (fun ty -> GpuVal.mk_gpu_vec ty outputShape)
          outputTypes
      in
      let args = Array.of_list (xGpu :: yGpu :: outputVals) in
      let gridParams = {
          LibPQ.threads_x=16; threads_y=16; threads_z=1;
          grid_x=safe_div nx 16; grid_y=safe_div ny 16;
      }
      in
      LibPQ.launch_ptx compiledModule.Cuda.module_ptr fnName args gridParams;
      outputVals
    

let init () = 
  (* initialize GPU contexts and device info *) 
  LibPQ.cuda_init(); 
  HardwareInfo.hw_init()
  
let shutdown () = 
  for i = 0 to DynArray.length HardwareInfo.device_contexts - 1 do 
    Cuda.cuda_ctx_destroy (DynArray.get HardwareInfo.device_contexts i)
  done 
