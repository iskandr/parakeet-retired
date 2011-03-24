/*
 * Parallel Q Library
 *
 * (c) 2009 Eric Hielscher, Alex Rubinsteyn
 *
 * OCaml interface for native CUDA functions
 */

#include <cuda.h>
#include <stdint.h>
#include <stdio.h>

extern "C" {

#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

/* unit -> unit */
CAMLprim
value ocaml_cuda_init(value unit) {
  CAMLparam1 (unit);
  if (cuInit(0) != CUDA_SUCCESS) {
    exit (0);
  }
  CAMLreturn (Val_unit);
}

CAMLprim
value ocaml_cuda_init_runtime(void) {
  CAMLparam0();

  cudaEvent_t dummy;
  cudaEventCreate(&dummy);
  cudaEventDestroy(dummy);

  CAMLreturn(Val_unit);
}

/* unit -> int32 */
CAMLprim value ocaml_cuda_device_get_count(void) {
  CAMLparam0();
  int devicesCount = 0;
  cuDeviceGetCount(&devicesCount);
  CAMLreturn(Val_int(devicesCount));
}

/* int -> device_info */
value ocaml_cuda_device_get_properties(value idx) {
  CAMLparam1(idx);
  CAMLlocal1(ocaml_dev_info);

  // Get the device info from CUDA
  CUdevice device;
  cuDeviceGet(&device, Int_val(idx));
  CUdevprop dev_prop;
  cuDeviceGetProperties(&dev_prop, device);

  // Build the OCaml version of the device info
  ocaml_dev_info = caml_alloc_tuple(14);
  Store_field(ocaml_dev_info, 0,  Int_val(dev_prop.maxThreadsPerBlock));
  Store_field(ocaml_dev_info, 1,  Int_val(dev_prop.maxThreadsDim[0]));
  Store_field(ocaml_dev_info, 2,  Int_val(dev_prop.maxThreadsDim[1]));
  Store_field(ocaml_dev_info, 3,  Int_val(dev_prop.maxThreadsDim[2]));
  Store_field(ocaml_dev_info, 4,  Int_val(dev_prop.maxGridSize[0]));
  Store_field(ocaml_dev_info, 5,  Int_val(dev_prop.maxGridSize[1]));
  Store_field(ocaml_dev_info, 6,  Int_val(dev_prop.maxGridSize[2]));
  Store_field(ocaml_dev_info, 7,  Int_val(dev_prop.sharedMemPerBlock));
  Store_field(ocaml_dev_info, 8,  Int_val(dev_prop.totalConstantMemory));
  Store_field(ocaml_dev_info, 9,  Int_val(dev_prop.SIMDWidth));
  Store_field(ocaml_dev_info, 10, Int_val(dev_prop.memPitch));
  Store_field(ocaml_dev_info, 11, Int_val(dev_prop.regsPerBlock));
  Store_field(ocaml_dev_info, 12, Int_val(dev_prop.clockRate));
  Store_field(ocaml_dev_info, 13, Int_val(dev_prop.textureAlign));

  CAMLreturn(ocaml_dev_info);
}

CAMLprim
value ocaml_cuda_device_get_free_and_total_mem(void) {
  CAMLparam0();
  CAMLlocal1(ocaml_mem_info);
  size_t free, total;
  cudaMemGetInfo(&free, &total);
  ocaml_mem_info = caml_alloc_tuple(2);
  Store_field(ocaml_mem_info, 0, Val_int(free / 1048576));
  Store_field(ocaml_mem_info, 1, Val_int(total / 1048576));

  CAMLreturn(ocaml_mem_info);
}

/* int -> Int64.t */
CAMLprim value ocaml_cuda_ctx_create(value dev_num) {
  CAMLparam1(dev_num);
  CUcontext ctx = 0;
  CUresult result = cuCtxCreate(&ctx, 0, Int_val(dev_num));
  if (result != 0) {
    printf ("Error creating context on dev %d: %d \n",
            Int_val(dev_num), result);
    exit(1);
  }
  CAMLreturn(caml_copy_int64((int64_t)ctx));
}

/* Int64.t -> unit */
CAMLprim value ocaml_cuda_ctx_destroy(value ctx) {
  CAMLparam1(ctx);
  CUresult result = cuCtxDestroy((CUcontext)Int64_val(ctx));
  if (result != 0) {
    printf ("Error destroying context: %d \n", result);
    exit(1);
  }
  CAMLreturn(Val_unit);
}

CAMLprim
value ocaml_cuda_create_event(void) {
  CAMLparam0();

  cudaEvent_t event;
  cudaEventCreate(&event);

  CAMLreturn(caml_copy_int64((int64_t)event));
}

CAMLprim
value ocaml_cuda_destroy_event(value ocaml_event) {
  CAMLparam1(ocaml_event);

  cudaEvent_t event = (cudaEvent_t)Int64_val(ocaml_event);
  cudaEventDestroy(event);

  CAMLreturn(Val_unit);
}

CAMLprim
value ocaml_cuda_record_event(value ocaml_event) {
  CAMLparam1(ocaml_event);

  cudaEvent_t event = (cudaEvent_t)Int64_val(ocaml_event);
  // TODO: For now, only using stream 0
  cudaEventRecord(event, 0);

  CAMLreturn(Val_unit);
}

CAMLprim
value ocaml_cuda_stop_event_and_get_elapsed_time(value ocaml_start_event,
                                                 value ocaml_end_event) {
  CAMLparam2(ocaml_start_event, ocaml_end_event);

  cudaEvent_t start_event = (cudaEvent_t)Int64_val(ocaml_start_event);
  cudaEvent_t end_event = (cudaEvent_t)Int64_val(ocaml_end_event);

  // TODO: For now, only using stream 0
  cudaEventRecord(end_event, 0);
  cudaEventSynchronize(end_event);

  float t;
  cudaEventElapsedTime(&t, start_event, end_event);

  CAMLreturn(caml_copy_double((double)t));
}

/** Memory-related functions **/

/* int -> Int64.t */
CAMLprim value ocaml_cuda_malloc(value num_bytes)  {
  CAMLparam1(num_bytes);
  CUdeviceptr devPtr;
  int n = Int_val(num_bytes);
  CUresult result = cuMemAlloc(&devPtr, n);
  
  if (result == CUDA_ERROR_OUT_OF_MEMORY) { 
    CAMLreturn (copy_int64(0)); 
  } 
  else if (result != 0) {
    printf ("cuMemAlloc failed for %d bytes with error code: %d\n", n, result);
    exit(1);
  }
  CAMLreturn (copy_int64(devPtr));
}

/* Int64.t -> unit */
CAMLprim value ocaml_cuda_free(value gpu_ptr)  {
  CAMLparam1(gpu_ptr);
  CUresult result = cuMemFree(Int64_val(gpu_ptr));
  if (result != 0) {
    printf ("cuMemFree failed with error code: %d\n", result);
    exit(1);
  }
  CAMLreturn (Val_unit);
}

/* Int64.t -> Int64.t -> int -> unit */
CAMLprim
value ocaml_cuda_memcpy_to_device (value array,
                    value dev_ptr,
                    value num_bytes) {
  CAMLparam3(array, dev_ptr, num_bytes);
  void* source = (void*)Int64_val(array);
  CUdeviceptr dest = Int64_val(dev_ptr);
  CUresult result = cuMemcpyHtoD(dest, source, Int_val(num_bytes));
  if (result != 0) {
    printf ("Error copying memory from host to GPU: %d \n", result);
    exit(1);
  }
  CAMLreturn(Val_unit);
}

/* Int64.t -> Int64.t -> int -> unit */

CAMLprim
value ocaml_cuda_memcpy_device_to_device (value src,
                    value dst,
                    value num_bytes) {
  CAMLparam3(src, dst, num_bytes);
  CUdeviceptr source = Int64_val(src);
  CUdeviceptr dest = Int64_val(dst);
  
  CUresult result = cuMemcpyDtoD(dest, source, Int_val(num_bytes));
  if (result != 0) {
    printf ("Error copying memory from GPU to GPU: %d \n", result);
    exit(1);
  }
  CAMLreturn(Val_unit);
}

/* Int64.t -> Int64.t -> int -> unit */
CAMLprim
value ocaml_cuda_memcpy_host_to_symbol(value symbol,
                                       value src,
                                       value num_bytes,
                                       value ocaml_offset) {
  CAMLparam4(symbol, src, num_bytes, ocaml_offset);
  char *name = String_val(symbol);
  void *source = (void*)Int64_val(src);
  int nbytes = Int_val(num_bytes);
  int offset = Int_val(ocaml_offset);

  cudaError_t result = cudaMemcpyToSymbol(name, source, nbytes, offset,
                                          cudaMemcpyHostToDevice);
  if (result) {
    printf("Error copying from host to constant symbol %s: %d\n", name, result);
    exit(1);
  }

  CAMLreturn(Val_unit);
}

CAMLprim
value ocaml_cuda_memcpy_device_to_symbol(value modulePtr, 
                                       value symbol,
                                       value src,
                                       value num_bytes,
                                       value ocaml_offset) {
  CAMLparam5(modulePtr, symbol, src, num_bytes, ocaml_offset);
  CUmodule* cuModule = (CUmodule*)  Int64_val(modulePtr); 
  char *name = String_val(symbol);
  CUdeviceptr source = (CUdeviceptr) Int64_val(src);
  

  CUdeviceptr dest; 
  unsigned int bytes; 
  CUresult result = cuModuleGetGlobal  (&dest, &bytes, *cuModule, name);  

  int offset = Int_val(ocaml_offset);
  
  /*
    int nbytes = Int_val(num_bytes);
    cudaError_t result = cudaMemcpyToSymbol(name, source, nbytes, offset,
                                          cudaMemcpyDeviceToDevice);
  */
   
  if (result == CUDA_ERROR_NOT_FOUND) { 
    printf("Constant symbol %s not found!\n", name);
    exit(1);
  }
  else if (result) {
    printf("Error getting constant symbol %s: %d\n", name, result);
    exit(1);
  }
  
  result = cuMemcpyDtoD(dest+offset, source, Int_val(num_bytes));
  if (result) {
    printf("Error copying from device to constant symbol %s: %d\n", 
            name, result);
    exit(1);
  }

  CAMLreturn(Val_unit);
}


/* Int64.t -> Int64.t -> int -> unit */
CAMLprim
value ocaml_cuda_memcpy_to_host(value array, value dev_ptr, value num_bytes) {
  CAMLparam3(array,dev_ptr, num_bytes);
  void* dest = (void*)Int64_val(array);
  CUdeviceptr source = Int64_val(dev_ptr);
  CUresult result = cuMemcpyDtoH(dest, source, Int_val(num_bytes));
  if (result != 0) {
    printf ("Error copying memory from GPU to host: %d\n", result);
    exit(1);
  }
  CAMLreturn(Val_unit);
}

/* EEK:
   Access a GPU array element 
*/
CAMLprim
value ocaml_cuda_get_gpu_char_vec_elt(value ocaml_gpu_vec, value ocaml_id) {
  CAMLparam2(ocaml_gpu_vec, ocaml_id);
  int *gpu_vec = (int*)Int64_val(ocaml_gpu_vec);
  char cval = 0;
  int val = 0;
  cudaError_t rslt = cudaMemcpy(&cval, gpu_vec + Int_val(ocaml_id),
                                sizeof(char), cudaMemcpyDeviceToHost);
  if (rslt) {
    printf("Error getting element of gpu char vec: %d\n", rslt);
    exit(1);
  }
  val = (int)cval;
  CAMLreturn(Val_int(val));
}

CAMLprim
value ocaml_cuda_get_gpu_int_vec_elt(value ocaml_gpu_vec, value ocaml_id) {
  CAMLparam2(ocaml_gpu_vec, ocaml_id);
  int *gpu_vec = (int*)Int64_val(ocaml_gpu_vec);
  int val = 0;
  cudaError_t rslt = cudaMemcpy(&val, gpu_vec + Int_val(ocaml_id),
                                sizeof(int), cudaMemcpyDeviceToHost);
  if (rslt) {
    printf("Error getting element of gpu int vec: %d\n", rslt);
    exit(1);
  }
  CAMLreturn(Val_int(val));
}

/*external cuda_get_gpu_int32_vec_elt : GpuPtr.t -> int -> Int64.t
  = "ocaml_cuda_get_gpu_int32_vec_elt" 
*/ 
CAMLprim
value 
ocaml_cuda_get_gpu_int32_vec_elt(value ocaml_gpu_vec, value ocaml_id) {
  CAMLparam2(ocaml_gpu_vec, ocaml_id);
  int32_t *gpu_vec = (int*)Int64_val(ocaml_gpu_vec);
  int32_t val = 0;
  cudaError_t rslt = cudaMemcpy(&val, gpu_vec + Int_val(ocaml_id),
                                sizeof(int32_t), cudaMemcpyDeviceToHost);
  if (rslt) {
    printf("Error getting element of gpu int32 vec: %d\n", rslt);
    exit(1);
  }
  CAMLreturn(copy_int32(val));
}



/*external cuda_get_gpu_float32_vec_elt : GpuPtr.t -> int -> float 
  = "ocaml_cuda_get_gpu_float_vec_elt"
*/
CAMLprim
value 
ocaml_cuda_get_gpu_float32_vec_elt(value ocaml_gpu_vec, value ocaml_id) {
  CAMLparam2(ocaml_gpu_vec, ocaml_id);
  float *gpu_vec = (float*)Int64_val(ocaml_gpu_vec);
  float val = 0.0;
  cudaError_t rslt = cudaMemcpy(&val, gpu_vec + Int_val(ocaml_id),
                                sizeof(float), cudaMemcpyDeviceToHost);
  if (rslt) {
    printf("Error getting element of gpu float32 vec: %d\n", rslt);
    exit(1);
  }
  CAMLreturn(copy_double(val));
}

/* 
   Set a GPU array element 
*/ 

/*external cuda_set_gpu_int32_vec_elt : GpuPtr.t -> int -> Int64.t -> unit
*/ 
CAMLprim
void 
ocaml_cuda_set_gpu_int32_vec_elt(value ocaml_gpu_vec, value ocaml_id, 
                                     value ocaml_elt) {
                                     
  CAMLparam3(ocaml_gpu_vec, ocaml_id, ocaml_elt);
  int32_t gpu_vec = Int64_val(ocaml_gpu_vec);
  int32_t arr[1]; 
  arr[0] = Int32_val(ocaml_elt);
  CUresult result = cuMemcpyHtoD(gpu_vec, arr, sizeof(int32_t));
  if (result) {
    printf("Error setting int32 element of gpu int32 vec: %d\n", result);
    exit(1);
  }
  CAMLreturn0;
}

CAMLprim
void 
ocaml_cuda_set_gpu_float32_vec_elt(value ocaml_gpu_vec, value ocaml_id, 
                                     value ocaml_elt) {
  CAMLparam3(ocaml_gpu_vec, ocaml_id, ocaml_elt);
  int32_t gpu_vec = Int64_val(ocaml_gpu_vec);
  float arr[1]; 
  arr[0] = Double_val(ocaml_elt);
  CUresult result = cuMemcpyHtoD(gpu_vec, arr, sizeof(float));
  if (result) {
    printf("Error setting float32 element of gpu int vec: %d\n", result);
    exit(1);
  }
  CAMLreturn0;
}

/** Textures **/

CAMLprim
value ocaml_cuda_module_get_tex_ref(value ocaml_module_ptr, value ocaml_name) {
  CAMLparam2(ocaml_module_ptr, ocaml_name);

  CUtexref tex_ref;
  cuModuleGetTexRef(&tex_ref, *(CUmodule*)Int64_val(ocaml_module_ptr),
                    String_val(ocaml_name));

  CAMLreturn(caml_copy_int64((int64_t)tex_ref));
}

CAMLprim
value ocaml_cuda_bind_texture_1d(
    value tex_ref, value dev_ptr, value bytes, value kind) {
  CAMLparam4(tex_ref, dev_ptr, bytes, kind);

  // For now, we assume that the offset is 0
  CUtexref tex = (CUtexref)Int64_val(tex_ref);
  int c_kind = Int_val(kind);
  switch(c_kind) {
    case 0:
      cuTexRefSetFormat(tex, CU_AD_FORMAT_SIGNED_INT32, 1);
      break;
    case 1:
      cuTexRefSetFormat(tex, CU_AD_FORMAT_UNSIGNED_INT32, 1);
      break;
    case 2:
      cuTexRefSetFormat(tex, CU_AD_FORMAT_FLOAT, 1);
      break;
    default:
      printf("Unknown kind for 1D texture binding. Aborting.\n");
      exit(1);
  }
  cuTexRefSetAddress(0, tex, (CUdeviceptr)Int64_val(dev_ptr), Int_val(bytes));
  CAMLreturn(Val_unit);
}

#define SIGNED_CHANNEL_FORMAT 0
#define UNSIGNED_CHANNEL_FORMAT 1
#define FLOAT_CHANNEL_FORMAT 2 

CAMLprim
value ocaml_cuda_bind_texture_2d_std_channel(
    value tex_ref, value dev_ptr, value width, value height, value kind) {
  CAMLparam5(tex_ref, dev_ptr, width, height, kind);

  unsigned int c_width = Int_val(width);
  unsigned int c_height = Int_val(height);
  int c_kind = Int_val(kind);

  // For now, all supported types are 32 bits.
  unsigned int pitch = c_width * 4;
  CUDA_ARRAY_DESCRIPTOR desc;
  switch(c_kind) {
    case SIGNED_CHANNEL_FORMAT:
      desc.Format = CU_AD_FORMAT_SIGNED_INT32;
      break;
    case UNSIGNED_CHANNEL_FORMAT:
      desc.Format = CU_AD_FORMAT_UNSIGNED_INT32;
      break;
    case FLOAT_CHANNEL_FORMAT:
      desc.Format = CU_AD_FORMAT_FLOAT;
      break;
    default:
      printf("[ocaml_cuda_bind_texture_2d_std_channel] unsupported format\n");
      exit(1);
  }
  desc.NumChannels = 1;
  desc.Width = c_width;
  desc.Height = c_height;

  // For now, we assume that the offset is 0, and that we don't care about
  // whether we're normalized because we shouldn't be addressing out of bounds.
  cuTexRefSetAddress2D((CUtexref)Int64_val(tex_ref), &desc,
                       (CUdeviceptr)Int64_val(dev_ptr), pitch);
  
  CAMLreturn(Val_unit);
}

CAMLprim
value ocaml_unbind_texture(value tex_ref) {
  CAMLparam1(tex_ref);

  cudaUnbindTexture((textureReference*)Int64_val(tex_ref));
  
  CAMLreturn(Val_unit);
}

}
