(* pp: -parser o pa_macro.cmo *)

open Base 
open Printf
open GpuVal 

type t = {
  gpu_vals : (InterpVal.DataId.t, GpuVal.gpu_val) Hashtbl.t;
  host_vals : (InterpVal.DataId.t, HostVal.host_val) Hashtbl.t; 
  envs : InterpVal.t ID.Map.t Stack.t;
  frozen_gpu_ptrs :  Cuda.GpuPtr.t MutableSet.t; 
}

let create numvars = {
  gpu_vals = Hashtbl.create (2*numvars);
  host_vals = Hashtbl.create (2*numvars); 
  envs = Stack.create();
  frozen_gpu_ptrs = MutableSet.create 127 
}

let get_curr_env memState = 
  assert (not $ Stack.is_empty memState.envs); 
  Stack.top memState.envs  

let push_env memState : unit = 
  if Stack.is_empty memState.envs then 
    Stack.push ID.Map.empty memState.envs 
  else 
    (* copy the current environment *) 
    Stack.push (get_curr_env memState) memState.envs
      
let pop_env memState : unit =
  IFDEF DEBUG THEN 
    assert (not $ Stack.is_empty memState.envs); 
  ENDIF;   
  ignore (Stack.pop memState.envs) 
   
let env_lookup memState id =
  let env = get_curr_env memState in 
   if ID.Map.mem id env then ID.Map.find id env
   else 
     failwith $ 
       Printf.sprintf "[eval_value] variable %s not found!" (ID.to_str id) 

let env_set_multiple memState  ids vals = 
 let env = Stack.pop memState.envs in  
  IFDEF DEBUG THEN 
    assert (List.length ids = List.length vals); 
  ENDIF; 
  let env' = ID.Map.extend env ids vals in 
  Stack.push env' memState.envs  
  
let env_set memState id rhs =
  let env = Stack.pop memState.envs in  
  let env' = ID.Map.add id rhs env in 
  Stack.push env' memState.envs 

let add_host state hostVal = 
  let id = InterpVal.DataId.gen() in 
  Hashtbl.add state.host_vals id hostVal;
  InterpVal.Data id 
  
let add_gpu state gpuVal = 
  let id = InterpVal.DataId.gen() in 
  Hashtbl.add state.gpu_vals id gpuVal;
  InterpVal.Data id 



let is_on_gpu memState = function 
  | InterpVal.Data id -> Hashtbl.mem memState.gpu_vals id 
  | InterpVal.Scalar _ -> true
  (* even if all the array's elements are on the GPU, no guarantee 
     they are a contiguous chunk 
  *)
  | InterpVal.Array _ -> false

let is_on_host memState = function 
  | InterpVal.Data id -> Hashtbl.mem memState.host_vals id 
  | InterpVal.Scalar _ -> true
  | InterpVal.Array _ -> false
  

let get_scalar state = function 
  | InterpVal.Data id -> 
      if Hashtbl.mem state.host_vals id then 
      match Hashtbl.find state.host_vals id with 
        | HostVal.HostScalar n -> n
        | _ -> assert false 
      else ( match Hashtbl.find state.gpu_vals id with 
        | GpuVal.GpuScalar n -> n 
        | _ -> assert false
      )
 | InterpVal.Scalar n -> n 
 | InterpVal.Array _ -> failwith "An array? How did that happen?"



let rec get_shape memState interpVal = match interpVal with   
  | InterpVal.Scalar _ -> Shape.scalar_shape 
  | InterpVal.Data id ->   
      if is_on_gpu memState interpVal then
        let gpuVal = Hashtbl.find memState.gpu_vals id in 
        GpuVal.get_shape gpuVal  
      else (
        assert (is_on_host memState interpVal);
        let hostVal = Hashtbl.find memState.host_vals id in 
        HostVal.get_shape hostVal  
      )
  | InterpVal.Array arr -> 
      (* assume uniform arrays *)
      let eltShape = get_shape memState arr.(0) in
      Shape.append_dim (Array.length arr) eltShape 

let rec get_type memState interpVal = 
  match interpVal with  
  | InterpVal.Scalar n -> PQNum.type_of_num n 
  | InterpVal.Data id -> 
      if is_on_gpu memState interpVal then
        let gpuVal = Hashtbl.find memState.gpu_vals id in  
        GpuVal.get_type gpuVal  
      else (
        assert (is_on_host memState interpVal);
        let hostVal = Hashtbl.find memState.host_vals id in 
        HostVal.get_type hostVal  
      )
  | InterpVal.Array arr -> 
      (* for now, assume uniformity of element types *) 
      let eltT = get_type memState arr.(0) in 
      DynType.VecT eltT 


let rec sizeof memState interpVal = 
  let t = get_type memState interpVal in 
  match interpVal with  
  | InterpVal.Scalar n -> DynType.sizeof t  
  | InterpVal.Data _ -> 
      let s = get_shape memState interpVal in 
      Shape.nelts s * (DynType.sizeof (DynType.elt_type t))
  | InterpVal.Array arr ->  
      Array.fold_left (fun sum elt -> sum + sizeof memState elt) 0 arr 




let freeze_gpu_vec memState gpuVec = 
  MutableSet.add memState.frozen_gpu_ptrs gpuVec.GpuVal.vec_ptr; 
  MutableSet.add memState.frozen_gpu_ptrs gpuVec.GpuVal.vec_shape_ptr

let unfreeze_gpu_vec memState gpuVec = 
  MutableSet.remove memState.frozen_gpu_ptrs gpuVec.GpuVal.vec_ptr; 
  MutableSet.remove memState.frozen_gpu_ptrs gpuVec.GpuVal.vec_shape_ptr

let unfreeze_gpu_vecs memState gpuVecs = 
  List.iter (unfreeze_gpu_vec memState) gpuVecs 

(* TODO: put a garbage collector around these! *) 
let mk_gpu_vec  memState ?(freeze=false) t shape =
  let destVec = Alloc.alloc_gpu_vec t shape in
  if freeze then freeze_gpu_vec memState destVec; 
  (* data id is never returned, only used to 
     track this array for garbage collection 
   *)
  let gpuVal = (GpuVal.GpuArray destVec) in
  let id = InterpVal.DataId.gen() in 
  Hashtbl.add memState.gpu_vals id gpuVal;
  destVec  
  
let mk_gpu_val memState ?(freeze=false) t shape = 
  IFDEF DEBUG THEN 
    assert (DynType.is_vec t); 
  ENDIF; 
  let gpuVec = mk_gpu_vec memState ~freeze t shape in 
  GpuVal.GpuArray gpuVec   
  

let rec get_gpu memState = function 
  | InterpVal.Data id -> 
    if Hashtbl.mem memState.gpu_vals id then
      Hashtbl.find memState.gpu_vals id
    else (

      let hostVal = Hashtbl.find memState.host_vals id in
      IFDEF DEBUG THEN 
        Printf.printf "Sending to GPU: --%s\n" (HostVal.to_str hostVal); ENDIF; 
        let gpuVal = Alloc.to_gpu hostVal in
        Hashtbl.replace memState.gpu_vals id gpuVal; 
        gpuVal
   )
  | InterpVal.Scalar n -> GpuVal.GpuScalar n
  (* WARNING: This is essentially a memory leak, since we leave 
     no data id associated with the gpu memory allocated here 
   *)   
  | InterpVal.Array arr ->
      (* for now, assume all rows are of uniform type/size *)
      let nrows = Array.length arr in   
      let elt = arr.(0) in 
      let eltSize = sizeof memState elt in
      let eltType = get_type memState elt in 
      let eltShape = get_shape memState elt in  
      let nbytes = nrows * eltSize in
      let finalType = DynType.VecT eltType in 
      let finalShape = Shape.append_dim nrows eltShape in 
      let destVec = mk_gpu_vec memState finalType finalShape in
      let destPtr = destVec.GpuVal.vec_ptr in 
      IFDEF DEBUG THEN 
        Printf.printf "[MemoryState] Transferring interpreter array to GPU\n";
        Printf.printf "[MemoryState] -- elts = %s \n" 
            (String.concat ", " (List.map InterpVal.to_str (Array.to_list arr)))
        ;  
        Printf.printf 
          "[MemoryState] -- elt size: %d, elt type: %s, elt shape: %s\n" 
            eltSize
            (DynType.to_str eltType)
            (Shape.to_str eltShape)
        ;
        Printf.printf 
          "[MemoryState] -- total size: %d, final type : %s,  final shape: %s\n"
          nbytes
          (DynType.to_str finalType)
          (Shape.to_str finalShape)
        ;  
      ENDIF; 
      for i = 0 to nrows - 1 do 
        let currPtr = Int64.add destPtr (Int64.of_int $ i * eltSize) in 
        match arr.(i) with 
          | InterpVal.Scalar (PQNum.Int32 i32) -> 
              Cuda.cuda_set_gpu_int32_vec_elt currPtr 0 i32 
          | InterpVal.Scalar (PQNum.Float32 f32) -> 
              Cuda.cuda_set_gpu_float32_vec_elt currPtr 0 f32 
          | InterpVal.Data id -> 
              if Hashtbl.mem memState.gpu_vals id then 
                let eltVal = Hashtbl.find memState.gpu_vals id in 
                let eltPtr = GpuVal.get_ptr eltVal in 
                Cuda.cuda_memcpy_device_to_device currPtr eltPtr eltSize 
              else
                let eltHostVal = Hashtbl.find memState.host_vals id in 
                let eltHostPtr = HostVal.get_ptr eltHostVal in 
                Cuda.cuda_memcpy_to_device eltHostPtr currPtr eltSize  
          | _ -> assert false 
       done; 
       GpuVal.GpuArray destVec

let rec get_host state interpVal = 
  match interpVal with  
  | InterpVal.Data id -> 
    if Hashtbl.mem state.host_vals id then
      Hashtbl.find state.host_vals id
    else (
      IFDEF DEBUG THEN 
        Printf.printf 
          "[MemoryState->get_host] Initiating transfer from GPU to host\n%!"
        ;
      ENDIF; 
      let gpuVal = Hashtbl.find state.gpu_vals id in
      let hostVal = Alloc.from_gpu gpuVal in
      Hashtbl.replace state.host_vals id hostVal;
      IFDEF DEBUG THEN
        Printf.printf "[MemoryState->get_host] Got %s \n" 
          (HostVal.to_str hostVal);
      ENDIF;
      hostVal
    )
  | InterpVal.Scalar n -> HostVal.HostScalar n 
  | InterpVal.Array arr ->
      HostVal.HostBoxedArray (Array.map (get_host state) arr)


  (* slice on the GPU or CPU? *) 
let slice memState arr idx = match arr with 
  | InterpVal.Scalar _ -> failwith "[MemoryState] Can't slice a scalar"
  | InterpVal.Array arr -> arr.(idx)
  | InterpVal.Data id -> 
      if Hashtbl.mem memState.gpu_vals id then 
        let gpuVal = Hashtbl.find memState.gpu_vals id in 
        add_gpu memState (GpuVal.slice gpuVal idx) 
      else 
        let hostVal = Hashtbl.find memState.host_vals id in 
        add_host memState (HostVal.slice hostVal idx)   
 
let free_gpu state id =
  if Hashtbl.mem state.gpu_vals id then (
    Alloc.delete_gpu_val (Hashtbl.find state.gpu_vals id);
    Hashtbl.remove state.host_vals id
  )

let free_all_gpu state =
  Hashtbl.iter (fun _ gpuVal -> Alloc.delete_gpu_val gpuVal) state.gpu_vals;
  Hashtbl.clear state.gpu_vals


let free_host state id =
  if Hashtbl.mem state.host_vals id then begin
    Alloc.delete_host_val (Hashtbl.find state.host_vals id);
    Hashtbl.remove state.host_vals id
  end

(* free identifier on both host and gpu *) 
let free state id = free_gpu state id; free_host state id

