(* pp: -parser o pa_macro.cmo *)

open Base 
open Printf
open InterpVal
open HostVal  
open GpuVal 


type t = {
  gpu_data : (DataId.t, GpuVal.gpu_vec) Hashtbl.t;
  host_data : (DataId.t, HostVal.host_array) Hashtbl.t; 
  
  envs :  InterpVal.t ID.Map.t Stack.t; 
  
  free_gpu_ptrs : (int, Cuda.GpuPtr.t) Hashtbl.t; 
  free_host_ptrs : (int, Cuda.HostPtr.t) Hashtbl.t;
  
  mutable frozen_gpu_ptrs :  Cuda.GpuPtr.Set.t;  
  mutable frozen_host_ptrs : Cuda.HostPtr.Set.t;   
}

let create size = 
  let n = 3*size + 1 in 
  {
    gpu_data = Hashtbl.create n; 
    host_data = Hashtbl.create n; 
       
    envs = Stack.create();
    
    free_gpu_ptrs = Hashtbl.create n; 
    free_host_ptrs = Hashtbl.create n;
    
    frozen_gpu_ptrs = Cuda.GpuPtr.Set.empty;
    frozen_host_ptrs = Cuda.HostPtr.Set.empty; 
  
  }

module Env = struct 
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
end
include Env 



module GcHelpers = struct
  let rec extend_set dataIdSet = function 
    | Data id -> DataId.Set.add id dataIdSet
    | Array arr -> Array.fold_left extend_set dataIdSet arr
    | Scalar _ -> dataIdSet

  let find_referenced_data env =
    ID.Map.fold 
      (fun _ interpVal dataIdSet -> extend_set dataIdSet interpVal)
      env 
      DataId.Set.empty 

  let liberate_gpu_vec memState gpuVec =
    (* can't free pointers which are offsets from 
       some other allocation
    *) 
    if gpuVec.vec_slice_start = None then ( 
      let dataPtr = gpuVec.vec_ptr in
      if not (Cuda.GpuPtr.Set.mem dataPtr memState.frozen_gpu_ptrs) then ( 
        let dataSize = gpuVec.vec_nbytes in 
        Hashtbl.add memState.free_gpu_ptrs dataSize dataPtr 
      ); 
      let shapePtr = gpuVec.vec_shape_ptr in
      if not (Cuda.GpuPtr.Set.mem shapePtr memState.frozen_gpu_ptrs) then (
        let shapeSize = gpuVec.vec_shape_nbytes in
        Hashtbl.add memState.free_gpu_ptrs shapeSize shapePtr
      )
    )
  let liberate_host_vec memState hostVec =
    if hostVec.slice_start = None then ( 
      let dataPtr = hostVec.ptr in
      if not (Cuda.HostPtr.Set.mem dataPtr memState.frozen_host_ptrs) then (
        let dataSize = hostVec.nbytes in  
        Hashtbl.add memState.free_host_ptrs dataSize dataPtr
      )
    ) 
  
  (* populates free_gpu_ptrs and free_host_ptrs with unused data *) 
  let liberate_data memState markedSet = 
    let gpu_helper dataId gpuVec = 
      if not $ (DataId.Set.mem dataId markedSet) 
      then liberate_gpu_vec memState gpuVec
    in 
    let host_helper dataId hostVec = 
      if not $ (DataId.Set.mem dataId markedSet) 
      then liberate_host_vec memState hostVec      
    in 
    Hashtbl.iter gpu_helper memState.gpu_data; 
    Hashtbl.iter host_helper memState.host_data 

end
  
  

let rec add_host memState = function 
  | HostVal.HostScalar n -> InterpVal.Scalar n 
  | HostVal.HostArray  hostVec -> 
      let id = DataId.gen() in 
      Hashtbl.add memState.host_data id hostVec;
      InterpVal.Data id 
  | HostVal.HostBoxedArray boxedArray -> 
      let interpValArray = Array.map (add_host memState) boxedArray in 
      InterpVal.Array interpValArray 
  
let add_gpu memState = function 
  | GpuVal.GpuArray gpuVec ->  
      let id = DataId.gen() in 
      Hashtbl.add memState.gpu_data id gpuVec;
      InterpVal.Data id
  | GpuVal.GpuScalar n -> InterpVal.Scalar n  


let is_on_gpu memState = function 
  | InterpVal.Data id -> Hashtbl.mem memState.gpu_data id 
  | InterpVal.Scalar _ -> true
  (* even if all the array's elements are on the GPU, no guarantee 
     they are a contiguous chunk 
  *)
  | InterpVal.Array _ -> false

let is_on_host memState = function 
  | InterpVal.Data id -> Hashtbl.mem memState.host_data id 
  | InterpVal.Scalar _ -> true
  | InterpVal.Array _ -> false
  

let get_scalar state = function 
 | InterpVal.Scalar n -> n
 | InterpVal.Data id -> assert false   
 | InterpVal.Array _ -> failwith "An array? How did that happen?"

let rec get_shape memState interpVal = match interpVal with   
  | InterpVal.Scalar _ -> Shape.scalar_shape 
  | InterpVal.Data id ->   
      if is_on_gpu memState interpVal then
        let gpuVec = Hashtbl.find memState.gpu_data id in
        gpuVec.vec_shape 
      else (
        IFDEF DEBUG THEN 
          assert (is_on_host memState interpVal);
         ENDIF; 
        let hostVec = Hashtbl.find memState.host_data id in
        hostVec.shape  
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
        let gpuVec = Hashtbl.find memState.gpu_data id in  
        gpuVec.vec_t   
      else (
        assert (is_on_host memState interpVal);
        let hostVec = Hashtbl.find memState.host_data id in
        hostVec.host_t  
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

module Freeze = struct 
  let freeze_gpu_vec memState gpuVec =
    let vecPtr = gpuVec.GpuVal.vec_ptr in
    let shapePtr = gpuVec.GpuVal.vec_shape_ptr in 
    let oldSet = memState.frozen_gpu_ptrs in  
    memState.frozen_gpu_ptrs <- 
      Cuda.GpuPtr.Set.add vecPtr (Cuda.GpuPtr.Set.add shapePtr oldSet)
  
  let freeze_gpu_vecs memState gpuVecs = 
    List.iter (freeze_gpu_vec memState) gpuVecs 
      
  let unfreeze_gpu_vec memState gpuVec =
    let oldSet = memState.frozen_gpu_ptrs in 
    let vecPtr = gpuVec.GpuVal.vec_ptr in 
    let shapePtr = gpuVec.GpuVal.vec_shape_ptr in
    memState.frozen_gpu_ptrs <- 
      Cuda.GpuPtr.Set.remove vecPtr (Cuda.GpuPtr.Set.remove shapePtr oldSet)
     
    
  let unfreeze_gpu_vecs memState gpuVecs = 
    List.iter (unfreeze_gpu_vec memState) gpuVecs 

  let freeze_host_vec memState hostVec = 
    memState.frozen_host_ptrs <- 
      Cuda.HostPtr.Set.add hostVec.ptr memState.frozen_host_ptrs

  let unfreeze_host_vec memState hostVec = 
    memState.frozen_host_ptrs <- 
      Cuda.HostPtr.Set.remove hostVec.ptr memState.frozen_host_ptrs


  let rec freeze memState = function 
    | InterpVal.Data id ->
      if Hashtbl.mem memState.host_data id then 
        freeze_host_vec memState (Hashtbl.find memState.host_data id)
      ;
      if Hashtbl.mem memState.gpu_data id then 
        freeze_gpu_vec memState (Hashtbl.find memState.gpu_data id)
    | InterpVal.Scalar _ -> () 
    | InterpVal.Array arr -> Array.iter (freeze memState) arr 

  let freeze_list memState vals = List.iter (freeze memState) vals 

  let rec unfreeze memState = function 
    | InterpVal.Data id ->
      if Hashtbl.mem memState.host_data id then 
        unfreeze_host_vec memState (Hashtbl.find memState.host_data id)
      ;
      if Hashtbl.mem memState.gpu_data id then 
        unfreeze_gpu_vec memState (Hashtbl.find memState.gpu_data id)
    | InterpVal.Scalar _ -> () 
    | InterpVal.Array arr -> Array.iter (unfreeze memState) arr 

  let unfreeze_list memState vals = List.iter (unfreeze memState) vals 
end 
include Freeze 

(* TODO: put a garbage collector around these! *) 
let mk_gpu_vec  memState ?(freeze=false) t shape =
  let destVec = Alloc.alloc_gpu_vec t shape in
  if freeze then freeze_gpu_vec memState destVec; 
  (* data id is never returned, only used to 
     track this array for garbage collection 
   *)
  let id = DataId.gen() in 
  Hashtbl.add memState.gpu_data id destVec;
  destVec  
  
let mk_gpu_val memState ?(freeze=false) t shape = 
  IFDEF DEBUG THEN 
    assert (DynType.is_vec t); 
  ENDIF; 
  let gpuVec = mk_gpu_vec memState ~freeze t shape in 
  GpuVal.GpuArray gpuVec   
  

let rec get_gpu memState = function 
  | InterpVal.Data id -> 
    if Hashtbl.mem memState.gpu_data id then
      GpuVal.GpuArray (Hashtbl.find memState.gpu_data id)
    else (
      let hostVec = Hashtbl.find memState.host_data id in
      IFDEF DEBUG THEN 
        Printf.printf "[MemoryState] Sending to GPU: --%s\n" 
          (HostVal.host_vec_to_str hostVec);
       ENDIF; 
      let gpuVec = Alloc.vec_to_gpu hostVec in 
      Hashtbl.replace memState.gpu_data id gpuVec; 
      GpuVal.GpuArray gpuVec
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
      let destPtr = destVec.vec_ptr in 
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
              if Hashtbl.mem memState.gpu_data id then 
                let eltVec = Hashtbl.find memState.gpu_data id in 
                IFDEF DEBUG THEN 
                  Printf.printf 
                    "[MemoryState] copying array elt to gpu address %Ld: %s\n"
                    currPtr
                    (GpuVal.gpu_vec_to_str eltVec)
                ENDIF; 
                Cuda.cuda_memcpy_device_to_device eltVec.vec_ptr currPtr eltSize 
              else
                let eltHostVec = Hashtbl.find memState.host_data id in
                IFDEF DEBUG THEN 
                  Printf.printf 
                    "[MemoryState] copying array elt to gpu address %Ld: %s\n"
                    currPtr
                    (HostVal.host_vec_to_str eltHostVec)
                ENDIF;  
                Cuda.cuda_memcpy_to_device eltHostVec.ptr currPtr eltSize  
          | _ -> assert false 
       done; 
       GpuVal.GpuArray destVec

let rec get_host state interpVal = 
  match interpVal with  
  | InterpVal.Data id -> 
    if Hashtbl.mem state.host_data id then
      HostVal.HostArray (Hashtbl.find state.host_data id)
    else (
      IFDEF DEBUG THEN 
        Printf.printf 
          "[MemoryState->get_host] Initiating transfer from GPU to host\n%!"
        ;
      ENDIF; 
      let gpuVec = Hashtbl.find state.gpu_data id in
      let hostVec = Alloc.vec_from_gpu gpuVec in
      Hashtbl.replace state.host_data id hostVec;
      IFDEF DEBUG THEN
        Printf.printf "[MemoryState->get_host] Got %s \n"
          (HostVal.host_vec_to_str hostVec);  
      ENDIF;
      HostVal.HostArray hostVec
    )
  | InterpVal.Scalar n -> HostVal.HostScalar n 
  | InterpVal.Array arr ->
      HostVal.HostBoxedArray (Array.map (get_host state) arr)


  (* slice on the GPU or CPU? *) 
let slice memState arr idx = match arr with 
  | InterpVal.Scalar _ -> failwith "[MemoryState] Can't slice a scalar"
  | InterpVal.Array arr -> arr.(idx)
  | InterpVal.Data id -> 
      if Hashtbl.mem memState.gpu_data id then 
        let gpuVec = Hashtbl.find memState.gpu_data id in        
        add_gpu memState (GpuVal.slice_vec gpuVec idx) 
      else 
        let hostVec = Hashtbl.find memState.host_data id in 
        add_host memState (HostVal.slice_vec hostVec idx)   
 
