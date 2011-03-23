(* pp: -parser o pa_macro.cmo *)

open Base


type 'a memspace = { 
  alloc : int -> 'a; 
  delete : 'a -> unit; 
  free_ptrs : (int, 'a list) Hashtbl.t; 
  name : string; 
} 

let create_memspace name allocFn deleteFn =  { 
  free_ptrs = Hashtbl.create 1001;
  alloc = allocFn; 
  delete = deleteFn;  
  name = name; 
}; 

external c_malloc_impl : int -> Int64.t = "ocaml_malloc"

exception HostOutOfMemory
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

let add_to_free_ptrs memSpace size ptr =
  let freeList = Hashtbl.find_default memSpace.free_ptrs size [] in 
  Hashtbl.replace memSpace.free_ptrs size (ptr::freeList)        
       

open GpuVal 

let delete_gpu_vec gpuMemSpace gpuVec = 
  if gpuVec.vec_slice_start = None && gpuVec.vec_nbytes > 0 then (
  IFDEF DEBUG THEN 
    Printf.printf 
      "[Alloc] Flushing gpu vec of size %d, shape %s, type %s @ %Lx\n%!" 
      gpuVec.vec_nbytes
      (Shape.to_str gpuVec.vec_shape)
      (DynType.to_str gpuVec.vec_t)
      gpuVec.vec_ptr
      ; 
      Printf.printf "[Alloc] -- %s\n%!" (GpuVal.gpu_vec_to_str gpuVec); 
    ENDIF; 
    gpuMemSpace.delete gpuVec.vec_ptr; 
    if gpuVec.vec_shape_nbytes > 0 then 
      gpuMemSpace.delete gpuVec.vec_shape_ptr
  )      