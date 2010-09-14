open Base 
open Printf 

exception InvalidGpuArgs 

let sizeof ty shape =
  DynType.sizeof (DynType.elt_type ty) * Shape.nelts shape  


let mk_cuda_module ptxList threadsPerBlock = 
  let ptxMap = PMap.of_enum (List.enum ptxList) in 
  let ptxModule = {
    Ptx.kernels = ptxMap;   
    compute_capability = Ptx.SM_13
  } 
  in 
  let ptxStr = Ptx.ptx_module_to_str ptxModule in
  print_string ptxStr;
  flush stdout; 
  let modulePtr = LibPQ.compile_module ptxStr threadsPerBlock in
  { 
    Cuda.module_ptr = modulePtr;
    kernel_names =  List.of_enum (PMap.keys ptxMap);
    threads_per_block =  threadsPerBlock
  } 

let mapPrefix = "map_kernel"

let compile_map payload argTypes retTypes = 
  let mapThreadsPerBlock = 128 in 
  (* converting payload to Imp *) 
  let impPayload = SSA_to_Imp.translate payload in
  (* generating Imp kernel w/ embedded payload *)
  let impfn = 
    ImpGenMap.gen_map 
      impPayload 
      mapThreadsPerBlock 
      (Array.of_list argTypes) 
      (Array.of_list retTypes) 
  in 
  let ptx = ImpToPtx.translate_kernel impfn in
  let name = mapPrefix ^ (string_of_int (ID.gen())) in
  mk_cuda_module [name,ptx] mapThreadsPerBlock 

let allPairsPrefix = "all_pairs_kernel"

let compile_all_pairs payload argTypes retType =
  match argTypes with 
    | [t1; t2] ->  
      let threadsPerBlock = 128 in 
      let impPayload = SSA_to_Imp.translate payload in
      let impfn = ImpGenAllPairs.gen_all_pairs_2d_tiled impPayload t1 t2 retType in 
      let ptx = ImpToPtx.translate_kernel impfn in
      let name = allPairsPrefix ^ (string_of_int (ID.gen())) in  
      mk_cuda_module [name, ptx] threadsPerBlock  
    | _ -> failwith "[compile_all_pairs] invalid argument types "      

let run_map compiledModule gpuVals outputTypes = 
  let shapes = List.map GpuVal.get_shape gpuVals in 
  match compiledModule.Cuda.kernel_names with 
    | [fnName] -> 
      let maxShape = (match Shape.max_shape_list shapes with
       | None -> raise InvalidGpuArgs
       | Some maxShape -> maxShape 
       )
      in
      let outputVals = 
        List.map 
          (fun ty ->  GpuVal.mk_gpu_vec ty maxShape (sizeof ty maxShape)) 
          outputTypes 
      in 
      let args = Array.of_list (gpuVals @ outputVals) in
      let outputElts = Shape.nelts maxShape in
      let gridParams = match 
        HardwareInfo.get_grid_params 
          ~device:0 
          ~block_size:compiledModule.Cuda.threads_per_block  
          outputElts  
        with 
        | Some gridParams -> gridParams
        | None -> 
         failwith (sprintf "Unable to get launch params for %d elts" outputElts)
      in   
      LibPQ.launch_ptx compiledModule.Cuda.module_ptr fnName args gridParams; 
      outputVals
    | _ -> failwith "expect one map kernel"

let init () = 
  (* initialize GPU contexts and device info *) 
  HardwareInfo.hw_init()
  
let shutdown () = 
  for i = 0 to DynArray.length HardwareInfo.device_contexts - 1 do 
    Cuda.cuda_ctx_destroy (DynArray.get HardwareInfo.device_contexts i)
  done 