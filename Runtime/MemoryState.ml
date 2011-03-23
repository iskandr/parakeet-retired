(* pp: -parser o pa_macro.cmo *)

open Base 
open Printf
open InterpVal
open HostVal  
open GpuVal 

type 'a memspace = { 
  alloc : int -> 'a; 
  delete : 'a -> unit; 
  free_ptrs : (int, 'a list) Hashtbl.t; 
  name : string; 
} 

exception HostOutOfMemory 
external c_malloc_impl : int -> Int64.t = "ocaml_malloc"
let c_malloc nbytes = 
  let ptr = c_malloc_impl nbytes in 
  if ptr = Int64.zero then raise HostOutOfMemory 
  else ptr 
   
external c_free : Int64.t -> unit = "ocaml_free"
external c_memcpy : Int64.t -> Int64.t -> int -> unit = "ocaml_memcpy"    
    
external get_array1_ptr 
  : ('a,'b,'c) Bigarray.Array1.t -> Int64.t = "get_bigarray_ptr"

external get_array2_ptr 
  : ('a,'b,'c) Bigarray.Array2.t -> Int64.t = "get_bigarray_ptr"

external get_array3_ptr 
  : ('a,'b,'c) Bigarray.Array3.t -> Int64.t = "get_bigarray_ptr"


type t = {
  envs :  InterpVal.t ID.Map.t Stack.t; 
  data_scopes : DataId.Set.t Stack.t; 
  refcounts : (DataId.t, int) Hashtbl.t;
   
  gpu_data : (DataId.t, GpuVal.gpu_vec) Hashtbl.t;
  host_data : (DataId.t, HostVal.host_array) Hashtbl.t; 
  
  gpu_mem : Cuda.GpuPtr.t memspace; 
  host_mem : Cuda.HostPtr.t memspace; 
}

let create () = 
  let memState = {
    envs = Stack.create();
    data_scopes = Stack.create ();
    refcounts = Hashtbl.create 1001; 
    gpu_data = Hashtbl.create 1001; 
    host_data = Hashtbl.create 1001; 
    
    gpu_mem = { 
      free_ptrs = Hashtbl.create 1001;
      alloc = Cuda.cuda_malloc; 
      delete = Cuda.cuda_free; 
      name = "GPU";
    }; 
    
    host_mem = { 
      free_ptrs = Hashtbl.create 1001; 
      alloc = c_malloc; 
      delete = c_free;
      name = "Host";  
    }
  }
  in 
  (* push an initial dataScope so that we can add data *)  
  Stack.push DataId.Set.empty memState.data_scopes; 
  memState  

let _ = Printexc.record_backtrace true 

let fresh_data_id memState =
  IFDEF DEBUG THEN 
    assert (not $ Stack.is_empty memState.data_scopes); 
  ENDIF; 
  let currDataScope = Stack.pop memState.data_scopes in 
  let id = DataId.gen() in 
  Stack.push (DataId.Set.add id currDataScope) memState.data_scopes; 
  Hashtbl.add memState.refcounts id 0; 
  id 
  
module Alloc = struct
   
  let find_free_ptr (freePtrs:(int, 'a list) Hashtbl.t) (nbytes:int) = 
  try (match Hashtbl.find freePtrs nbytes with 
        | p::ps -> 
           Hashtbl.replace freePtrs nbytes ps;  Some p 
        | [] -> assert false 
      )
  with _ -> None 

 let free_ptr_list memspace nbytes ptrs = 
      IFDEF DEBUG THEN 
        Printf.printf "[Alloc] Deallocating %d blocks of size %d: %s\n%!"
        (List.length ptrs)
        nbytes 
        (String.concat ", " (List.map (Printf.sprintf "%Lx")  ptrs))
      ENDIF; 
      List.iter memspace.delete ptrs

  let dealloc_free_ptrs memspace = 
    IFDEF DEBUG THEN 
       Printf.printf
         "[Alloc] Deallocating all free vectors in %s memspace\n" 
         memspace.name;
       Pervasives.flush_all(); 
    ENDIF; 
    Hashtbl.iter (free_ptr_list memspace) memspace.free_ptrs;   
    Hashtbl.clear memspace.free_ptrs    
          

  (* try to use a free pointer before allocating new space *) 
  let smart_alloc (memspace : 'a memspace) (nbytes : int) : 'a =
    match find_free_ptr memspace.free_ptrs nbytes with 
    | None -> 
      (try memspace.alloc nbytes 
        with _ -> 
          dealloc_free_ptrs memspace; 
          memspace.alloc nbytes
      )
    | Some ptr -> 
        IFDEF DEBUG THEN
          Printf.printf "[Alloc] Reusing %d bytes of %s memory at %Lx\n"
            nbytes
            memspace.name
            ptr
        ENDIF; 
        ptr 

   
  let delete_gpu_vec memState gpuVec = 
    if gpuVec.vec_slice_start = None && gpuVec.vec_nbytes > 0 then (
      IFDEF DEBUG THEN 
        Printf.printf "[Alloc] Flushing gpu vec of size %d, shape %s @ %Lx\n%!" 
          gpuVec.vec_nbytes
          (Shape.to_str gpuVec.vec_shape)
          gpuVec.vec_ptr
        ; 
        Printf.printf "[Alloc] -- %s\n%!" (GpuVal.gpu_vec_to_str gpuVec); 
      ENDIF; 
      memState.gpu_mem.delete gpuVec.vec_ptr; 
      if gpuVec.vec_shape_nbytes > 0 then 
        memState.gpu_mem.delete gpuVec.vec_shape_ptr
    )
    
  let flush_gpu memState = 
    Hashtbl.iter 
      (fun _ gpuVec -> delete_gpu_vec memState gpuVec) 
      memState.gpu_data; 
    Hashtbl.clear memState.gpu_data

  (* careful! have to 
      (1) not delete slices (since the underlying data may referenced elsewhere
      (2) make sure these pointers are also removed from free_ptrs 
   *) 
  (*
  let delete_data memState dataId = 
    if Hashtbl.mem memState.gpu_data dataId then (
      let gpuVec = Hashtbl.find memState.gpu_data dataId in
      memState.gpu_mem.delete gpuVec.vec_ptr; 
      memState.gpu_mem.delete gpuVec.vec_shape_ptr;
      Hashtbl.remove memState.gpu_data dataId
    );
    if Hashtbl.mem memState.host_data dataId then (
      let hostVec = Hashtbl.find memState.host_data dataId in  
      memState.host_mem.delete hostVec.ptr;
      Hashtbl.remove memState.host_data dataId
    )
  *) 
  (* 
    TODO: 
      if gpu alloc still fails after ordinary garbage collection,
      bring some data back to the CPU. This is tricky since we have to 
      make sure to not bring back anything being used in the current 
      operation. Still not sure of the best way to do this.  
  *)        
  let alloc_gpu memState nbytes = smart_alloc memState.gpu_mem nbytes  
  let alloc_host memState nbytes = smart_alloc memState.host_mem nbytes 

  let calc_nbytes len ty shape =
    let eltT = DynType.elt_type ty in
    let eltSize = DynType.sizeof eltT in
    len * eltSize 

  (* send a shape vector the gpu *) 
  let shape_to_gpu memState shape =
    let shapeBytes = Shape.nbytes shape in
    IFDEF DEBUG THEN 
      Printf.printf "Sending shape to GPU: %s (%d bytes)\n"
        (Shape.to_str shape)
        shapeBytes
    ENDIF;
    let shapeDevPtr = alloc_gpu memState shapeBytes in
    let shapeHostPtr = get_array1_ptr $ Shape.to_c_array shape in
    Cuda.cuda_memcpy_to_device shapeHostPtr shapeDevPtr shapeBytes;
    shapeDevPtr, shapeBytes

  let mk_gpu_vec memState ?nbytes t shape =
    let nelts = Shape.nelts shape in 
    let dataBytes = 
      match nbytes with None -> calc_nbytes nelts t shape | Some sz -> sz 
    in
    let dataPtr = alloc_gpu memState dataBytes in 
    let shapePtr, shapeBytes = shape_to_gpu memState shape in 
    let gpuVec = {
      vec_ptr = dataPtr;
      vec_nbytes = dataBytes;
      vec_len = nelts;

      vec_shape_ptr = shapePtr;
      vec_shape_nbytes = shapeBytes;

      vec_shape = shape;
      vec_t = t;
      vec_slice_start = None; 
    }
    in 
    let id = fresh_data_id memState in 
    Hashtbl.add memState.gpu_data id gpuVec;
    IFDEF DEBUG THEN
      Printf.printf "[Alloc] Created %s\n"  (GpuVal.gpu_vec_to_str gpuVec);
      Pervasives.flush_all(); 
    ENDIF;  
    gpuVec   

  let mk_gpu_val memState t shape = 
    IFDEF DEBUG THEN assert (DynType.is_vec t); ENDIF; 
    let gpuVec = mk_gpu_vec memState t shape in 
    GpuVal.GpuArray gpuVec   

  let mk_host_vec memState ?nbytes ty shape =
    let len = Shape.nelts shape in  
    let nbytes = match nbytes with 
      | None ->
         let eltT = DynType.elt_type ty in 
         let eltSize = DynType.sizeof eltT in
         len * eltSize 
      | Some n -> n
    in  
    let ptr = alloc_host memState nbytes in 
    let hostVec = {
      ptr = ptr; 
      nbytes = nbytes; 
      host_t = ty; 
      shape = shape;
      slice_start = None  
    } 
    in 
    IFDEF DEBUG THEN
      Printf.printf "[Alloc] Created %s\n"  (HostVal.host_vec_to_str hostVec); 
      Pervasives.flush_all(); 
    ENDIF;  
    hostVec
    
  let vec_from_gpu memState gpuVec = 
    let dataHostPtr = alloc_host memState gpuVec.vec_nbytes in  
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
  

      
  let vec_to_gpu memState hostVec = 
    let gpuPtr = alloc_gpu memState hostVec.nbytes in  
    Cuda.cuda_memcpy_to_device hostVec.ptr gpuPtr hostVec.nbytes;
    let shapeDevPtr, shapeBytes = shape_to_gpu memState hostVec.shape in
    let gpuVec =  {
      vec_ptr = gpuPtr; 
      vec_nbytes = hostVec.nbytes;
      vec_len= Shape.nelts hostVec.shape; 
      
      vec_shape_ptr = shapeDevPtr;
      vec_shape_nbytes = shapeBytes; 
      
      vec_shape = hostVec.shape;
      vec_t = hostVec.host_t; 
      vec_slice_start=None; 
    }
    in 
    IFDEF DEBUG THEN 
      Printf.printf "[Alloc] vector sent to GPU: %s\n" 
        (GpuVal.gpu_vec_to_str gpuVec); 
    ENDIF;
    gpuVec    
end 
include Alloc   



module RefCounting = struct
  let add_to_free_ptrs memSpace size ptr =
    let freeList = Hashtbl.find_default memSpace.free_ptrs size [] in 
    Hashtbl.replace memSpace.free_ptrs size (ptr::freeList)  
 
  let free_gpu_data memState gpuVec = 
    IFDEF DEBUG THEN 
      Printf.printf "[RefCount] Adding to free list: %s\n" 
        (GpuVal.gpu_vec_to_str gpuVec); 
    ENDIF; 
    let nbytes = gpuVec.vec_nbytes in 
    if gpuVec.vec_slice_start = None && nbytes > 0 then (
      add_to_free_ptrs memState.gpu_mem nbytes gpuVec.vec_ptr;
      let shapeBytes = gpuVec.vec_shape_nbytes in  
      if shapeBytes > 0 then
        add_to_free_ptrs memState.gpu_mem shapeBytes gpuVec.vec_shape_ptr
    ) 
  let free_host_data memState hostVec = 
    IFDEF DEBUG THEN 
      Printf.printf "[RefCount] Adding to free list: %s\n" 
        (HostVal.host_vec_to_str hostVec);  
    ENDIF; 
    let nbytes = hostVec.nbytes in
    if hostVec.slice_start = None && nbytes > 0 then (
      add_to_free_ptrs memState.host_mem nbytes hostVec.ptr; 
    )
    
  (* add pointers associated with this data into free dicts *)
  (* TODO: what to do about data which other slices point into? 
         Currently it will get deleted, leaving slices dangling 
   *) 
  let free_data memState dataId =
    if Hashtbl.mem memState.gpu_data dataId then (  
      let gpuVec = Hashtbl.find memState.gpu_data dataId in 
      free_gpu_data memState gpuVec; 
      Hashtbl.remove memState.gpu_data dataId 
    );
    if Hashtbl.mem memState.host_data dataId then ( 
      let hostVec = Hashtbl.find memState.host_data dataId in 
      free_host_data memState hostVec;
      Hashtbl.remove memState.host_data dataId    
    )

  let inc_data_ref memState (dataId: DataId.t) : unit = 
    let nrefs = Hashtbl.find_default memState.refcounts dataId 0 in 
    Hashtbl.replace memState.refcounts dataId (nrefs + 1)

  let dec_data_ref memState (dataId: DataId.t) = 
    let nrefs = Hashtbl.find memState.refcounts dataId in 
    if nrefs <= 0 then (
      IFDEF DEBUG THEN 
        Printf.printf "[RefCount] %s has count %d, freeing\n"
          (DataId.to_str dataId)
          nrefs
      ENDIF; 
      free_data memState dataId;                
      Hashtbl.remove memState.refcounts dataId
    ) 
    else
      Hashtbl.replace memState.refcounts dataId (nrefs - 1)

  let rec inc_ref memState = function 
    | InterpVal.Data id -> inc_data_ref memState id
    | InterpVal.Scalar _ -> ()
    | InterpVal.Array arr -> Array.iter (inc_ref memState) arr

  let rec dec_ref memState = function 
    | InterpVal.Data id -> dec_data_ref memState id
    | InterpVal.Scalar _ -> ()
    | InterpVal.Array arr -> Array.iter (dec_ref memState) arr

end 
include RefCounting 

module Scope = struct 
  let get_curr_env memState = 
    IFDEF DEBUG THEN 
      assert (not $ Stack.is_empty memState.envs);
    ENDIF;  
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


  let dec_old_binding_value memState env id = 
    if ID.Map.mem id env then (
      IFDEF DEBUG THEN 
        Printf.printf "[RefCount] Decrementing reference count for %s"
          (ID.to_str id)
      ENDIF;   
      dec_ref memState (ID.Map.find id env)
    )
    
  let set_binding memState id rhs =
    (* be sure to increment ref counts first, to avoid deleting something 
       when it was also the old value of a variable 
    *) 
    inc_ref memState rhs; 
    IFDEF DEBUG THEN 
      assert (not $ Stack.is_empty memState.envs); 
    ENDIF; 
    let env = Stack.pop memState.envs in
    dec_old_binding_value memState env id; 
    let env' = ID.Map.add id rhs env in 
    Stack.push env' memState.envs 
  
  let set_bindings memState ids vals = 
    IFDEF DEBUG THEN 
      assert (List.length ids = List.length vals);
      assert (not $ Stack.is_empty memState.envs);  
    ENDIF;
    List.iter (inc_ref memState) vals;
    let env = Stack.pop memState.envs in
    List.iter (dec_old_binding_value memState env) ids;   
    let env' = ID.Map.extend env ids vals in
    Stack.push env' memState.envs  
  
  let push_data_scope memState = 
    Stack.push DataId.Set.empty memState.data_scopes

  let rec get_value_ids = function 
    | InterpVal.Data id -> [id]
    | InterpVal.Scalar _ -> []
    | InterpVal.Array arr ->
      Array.fold_left (fun acc x -> get_value_ids x @ acc) [] arr 


  let rec id_set_from_values memState = function 
    | [] -> DataId.Set.empty 
    | v::vs -> 
        let restSet = id_set_from_values memState vs in 
        let dataIds = get_value_ids v in 
        List.fold_left (fun acc id -> DataId.Set.add id acc) restSet dataIds

  let pop_data_scope ?(escaping_values=[]) memState = 
    let dataScope = Stack.pop memState.data_scopes in 
    let excludeSet = id_set_from_values memState escaping_values in  
    (* remove the escaping IDs from the scope before freeing its contents *)
    let prunedScope = DataId.Set.diff dataScope excludeSet in 
    DataId.Set.iter (dec_data_ref memState) prunedScope 

  let pop_scope ?(escaping_values=[]) memState = 
    pop_data_scope ~escaping_values memState; 
    pop_env memState
 
  let push_scope memState = 
    push_data_scope memState;
    push_env memState
end
include Scope 

let rec add_host memState = function 
  | HostVal.HostScalar n -> InterpVal.Scalar n 
  | HostVal.HostArray  hostVec -> 
      let id = fresh_data_id memState in 
      Hashtbl.add memState.host_data id hostVec;
      InterpVal.Data id 
  | HostVal.HostBoxedArray boxedArray -> 
      let interpValArray = Array.map (add_host memState) boxedArray in 
      InterpVal.Array interpValArray 
  
let add_gpu memState = function 
  | GpuVal.GpuArray gpuVec ->  
      let id = fresh_data_id memState in 
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
      let gpuVec = vec_to_gpu memState hostVec in 
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
                    "[MemoryState] copying array elt to gpu address %Lx: %s\n"
                    currPtr
                    (GpuVal.gpu_vec_to_str eltVec)
                ENDIF; 
                Cuda.cuda_memcpy_device_to_device eltVec.vec_ptr currPtr eltSize 
              else
                let eltHostVec = Hashtbl.find memState.host_data id in
                IFDEF DEBUG THEN 
                  Printf.printf 
                    "[MemoryState] copying array elt to gpu address %Lx: %s\n"
                    currPtr
                    (HostVal.host_vec_to_str eltHostVec)
                ENDIF;  
                Cuda.cuda_memcpy_to_device eltHostVec.ptr currPtr eltSize  
          | _ -> assert false 
       done; 
       GpuVal.GpuArray destVec

let rec get_host memState interpVal = 
  match interpVal with  
  | InterpVal.Data id -> 
    if Hashtbl.mem memState.host_data id then
      HostVal.HostArray (Hashtbl.find memState.host_data id)
    else (
      IFDEF DEBUG THEN 
        Printf.printf 
          "[MemoryState->get_host] Initiating transfer from GPU to host\n%!"
        ;
      ENDIF; 
      let gpuVec = Hashtbl.find memState.gpu_data id in
      let hostVec = vec_from_gpu memState gpuVec in
      Hashtbl.replace memState.host_data id hostVec;
      IFDEF DEBUG THEN
        Printf.printf "[MemoryState->get_host] Got %s \n"
          (HostVal.host_vec_to_str hostVec);  
      ENDIF;
      HostVal.HostArray hostVec
    )
  | InterpVal.Scalar n -> HostVal.HostScalar n 
  | InterpVal.Array arr ->
      HostVal.HostBoxedArray (Array.map (get_host memState) arr)


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
