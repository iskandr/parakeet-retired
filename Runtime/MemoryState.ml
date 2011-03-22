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
  data_scopes : DataId.Set.t Stack.t; 
  
  free_gpu_ptrs : (int, Cuda.GpuPtr.t) Hashtbl.t; 
  free_host_ptrs : (int, Cuda.HostPtr.t) Hashtbl.t;
  
  refcounts : (DataId.t, int) Hashtbl.t; 

  gpu_data_ids : (Cuda.GpuPtr.t, DataId.t) Hashtbl.t; 
  host_data_ids : (Cuda.HostPtr.t, DataId.t) Hashtbl.t; 
}

let create size = 
  {
    gpu_data = Hashtbl.create n; 
    host_data = Hashtbl.create n; 
       
    envs = Stack.create();
    data_scopes = Stack.create (); 

    free_gpu_ptrs = Hashtbl.create n; 
    free_host_ptrs = Hashtbl.create n;

    gpu_data_ids = Hashtbl.create n; 
    host_data_ids = Hashtbl.create n;
  }


module RefCounting = struct
  let inc_data_ref memState (dataId: DataId.t) = 
    let nrefs = Hashtbl.find_default memState.refs dataId 0 in 
    Hashtbl.replace memState.refs dataId nrefs + 1

  let dec_data_ref memState (dataId: DataId.t) = 
    let nrefs = Hashtbl.find memState.refs dataId in 
    if nrefs <= 0 then  
      (* delete any vectors associated with dataId *) 
      Hashtbl.remove memState.refs dataId 
    else
      Hashtbl.replace memState.refs dataId nrefs - 1

  let incMemoryState.dec_ref_gpu_vals P.memState !inputArgs; 

  let rec inc_ref memState = function 
    | InterpVal.Data id -> inc_data_ref memState id
    | InterpVal.Scalar _ -> ()
    | InterpVal.Array arr -> Array.iter (inc_ref memState) arr

  let rec dec_ref memState = function 
    | InterpVal.Data id -> dec_data_ref memState id
    | InterpVal.Scalar _ -> ()
    | InterpVal.Array arr -> Array.iter (dec_ref memState) arr

  let 
end 
include RefCounting 

module ManagedAlloc = struct 

  let calc_nbytes len ty shape =
    let eltT = DynType.elt_type ty in
    let eltSize = DynType.sizeof eltT in
    len * eltSize 

  let find_free_ptr (freePtrs:(int, 'a) Hashtbl.t) (nbytes:int) = 
    try 
      (
        match Hashtbl.find freePtrs nbytes with 
        | p::ps -> Some p 
        | [] -> assert false 
      )
    with _ -> None 

  let alloc_gpu memState nbytes = 
    match find_free_ptr memState.free_host_ptrs nbytes with 
    | Some ptr -> ptr 
    | None -> 
      try Cuda.malloc nbytes 
      with _ -> 
        (*  delete all free pointers *) 
        Cuda.malloc nbytes  

  let alloc_host memState nbytes = 
    alloc memState.free_gpu_ptrs c_malloc nbytes 

  let mk_gpu_vec memState ?(freeze=false) ?nbytes t shape =
    let nelts = Shape.nelts shape in 
    let dataBytes = 
      match nbytes with None -> calc_nbytes nelts t shape | Some sz -> sz 
    in
    let dataPtr = alloc_gpu dataBytes in 
    let shapeBytes = Shape.nbytes shape in 
    let shapePtr = alloc shapeBytes in 
    let shapePtr = match get_free_ptr memState.free_gpu_ptrs sz with 

    | Some ptr -> ptr 
    | None -> 
       let free, _ = Cuda.cuda_device_get_free_and_total_mem () in 
       if sz < free then 
         Alloc.alloc_gpu_vec sz t shape 
       in        
       let destVec = Alloc.alloc_gpu_vec sz t shape in
    if freeze then freeze_gpu_vec memState destVec; 
    (* data id is never returned, only used to 
       track this array for garbage collection 
     *)
    let id = DataId.gen() in 
    Hashtbl.add memState.gpu_data id destVec;
    destVec  

let alloc_gpu_vec ~nbytes ~len ty shape =
  let outputPtr = Cuda.cuda_malloc nbytes in
  let shapePtr, shapeSize = shape_to_gpu shape in
  let gpuVec = {
    vec_ptr = outputPtr;
    vec_nbytes = nbytes;
    vec_len = len;

    vec_shape_ptr = shapePtr;
    vec_shape_nbytes = shapeSize;

    vec_shape = shape;
    vec_t = ty;
    vec_slice_start = None; 

  } in 
  IFDEF DEBUG THEN
    Printf.printf "[Alloc] Created %s\n"  (GpuVal.gpu_vec_to_str gpuVec);
    Pervasives.flush_all(); 
  ENDIF;  
  gpuVec 
  

  
  let mk_gpu_val memState ?(freeze=false) t shape = 
    IFDEF DEBUG THEN 
      assert (DynType.is_vec t); 
    ENDIF; 
    let gpuVec = mk_gpu_vec memState ~freeze t shape in 
    GpuVal.GpuArray gpuVec   

  let mk_host_vec memState ?(freeze=false) t shape = 
     
     
  let vec_from_gpu gpuVec = 
    let dataHostPtr = c_malloc gpuVec.vec_nbytes in  
    Cuda.cuda_memcpy_to_host dataHostPtr gpuVec.vec_ptr gpuVec.vec_nbytes; 
    let hostVec = { 
      ptr = dataHostPtr; 
      nbytes = gpuVec.vec_nbytes;
      host_t = gpuVec.vec_t; 
      shape = gpuVec.vec_shape; 
      slice_start = None; 
    }
    in 
    IFDEF DEBUG THEN 
      Printf.printf "[Alloc] vector returned from GPU: %s\n"
        (HostVal.host_vec_to_str hostVec); 
    ENDIF;
    hostVec 
end 
include ManagedAlloc   


module Scope = struct 
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
   
  let lookup memState id =
    let env = get_curr_env memState in 
     if ID.Map.mem id env then ID.Map.find id env
     else 
       failwith $ 
         Printf.sprintf "[eval_value] variable %s not found!" (ID.to_str id) 

  let set_bindings memState ids vals = 
    let env = Stack.pop memState.envs in  
    IFDEF DEBUG THEN 
      assert (List.length ids = List.length vals); 
    ENDIF; 
    let env' = ID.Map.extend env ids vals in 
    Stack.push env' memState.envs  
  
  let set_binding memState id rhs =
    let env = Stack.pop memState.envs in  
    let env' = ID.Map.add id rhs env in 
    Stack.push env' memState.envs 

  let push_data_scope memState = 
    Stack.push DataId.Set memState.data_scopes
  
  let rec get_value_ids = function 
    | InterpVal.Data id -> [id]
    | InterpVal.Scalar _ -> []
    | InterpVal.Array arr ->
        List.concat (Array.to_list (Array.map get_value_ids arr))

  let rec id_set_from_values memState = function 
    | [] -> DataId.Set.empty 
    | v::vs -> 
        let restSet = id_set_from_values memState vs in 
        let dataIds = get_value_ids v in 
        List.fold_left (fun acc id -> DataId.Set.add id acc) restSet dataIds

  let pop_data_scope ?(escaping_value=[]) memState = 
    let dataScope = Stack.pop memState.data_scopes in 
    let excludeSet = id_set_from_values memState escaping_values in  
    (* remove the escaping IDs from the scope before freeing its contents *)
    let prunedScope = DataId.Set.diff dataScope escapingDataIds in 
    DataId.Set.iter (dec_data_ref memState) prunedScope 

  let pop_scope ?(escaping_values=[]) memState = 
    pop_data_scope ~escaping_values memState; 
    pop_env memState
 
  let push_scope memState = 
    push_data_scope memState;
    push_env memState
end
include Scope 


(*
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
*)  
  

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
 
