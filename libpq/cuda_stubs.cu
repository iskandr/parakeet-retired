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

/* int -> Int32.t */
CAMLprim value ocaml_cuda_malloc(value num_bytes)  {
  CAMLparam1(num_bytes);
  CUdeviceptr devPtr;
  int n = Int_val(num_bytes);
  CUresult result = cuMemAlloc(&devPtr, n);
  if (result != 0) {
    printf ("cuMemAlloc failed for %d bytes with error code: %d\n", n, result);
    exit(1);
  }
  CAMLreturn (copy_int32(devPtr));
}

/* Int32.t -> unit */
CAMLprim value ocaml_cuda_free(value gpu_ptr)  {
  CAMLparam1(gpu_ptr);
  CUresult result = cuMemFree(Int32_val(gpu_ptr));
  if (result != 0) {
    printf ("cuMemFree failed with error code: %d\n", result);
    exit(1);
  }
  CAMLreturn (Val_unit);
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
  Store_field(ocaml_mem_info, 0, Int_val(free));
  Store_field(ocaml_mem_info, 1, Int_val(total));

  CAMLreturn(ocaml_mem_info);
}

/* int -> Int64.t */
CAMLprim value ocaml_cuda_ctx_create(value dev_num) {
  CAMLparam1(dev_num);
  CUcontext ctx = 0;
  CUresult result = cuCtxCreate(&ctx, 0, Int_val(dev_num));
  if (result != 0) {
    printf ("Error creating context on dev %d: %d \n", Int_val(dev_num), result);
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
value ocaml_cuda_init_runtime(void) {
  CAMLparam0();

  cudaEvent_t dummy;
  cudaEventCreate(&dummy);
  cudaEventDestroy(dummy);

  CAMLreturn(Val_unit);
}

/* Int64.t -> Int32.t -> int -> unit */
CAMLprim
value ocaml_cuda_memcpy_to_device (value array,
                    value dev_ptr,
                    value num_bytes) {
  CAMLparam3(array, dev_ptr, num_bytes);
  void* source = (void*)Int64_val(array);
  CUdeviceptr dest = Int32_val(dev_ptr);
  CUresult result = cuMemcpyHtoD(dest, source, Int_val(num_bytes));
  if (result != 0) {
    printf ("Error copying memory from host to GPU: %d \n", result);
    exit(1);
  }
  CAMLreturn(Val_unit);
}

/* Int32.t -> Int32.t -> int -> unit */ 



CAMLprim
value ocaml_cuda_memcpy_device_to_device (value src,
                    value dst,
                    value num_bytes) {
  CAMLparam3(src, dst, num_bytes);
  CUdeviceptr source = Int32_val(src);
  CUdeviceptr dest = Int32_val(dst);
  
  CUresult result = cuMemcpyDtoD(dest, source, Int_val(num_bytes));
  if (result != 0) {
    printf ("Error copying memory from GPU to GPU: %d \n", result);
    exit(1);
  }
  CAMLreturn(Val_unit);
}


/* Int64.t -> Int32.t -> int -> unit */
CAMLprim
value ocaml_cuda_memcpy_to_host (value array,
                  value dev_ptr,
                  value num_bytes) {
  CAMLparam3(array,dev_ptr, num_bytes);
  void* dest = (void*)Int64_val(array);
  CUdeviceptr source = Int32_val(dev_ptr);
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
value ocaml_cuda_get_gpu_int_vec_elt(value ocaml_gpu_vec, value ocaml_id) {
  CAMLparam2(ocaml_gpu_vec, ocaml_id);
  int *gpu_vec = (int*)Int32_val(ocaml_gpu_vec);
  int32_t val = 0;
  cudaError_t rslt = cudaMemcpy(&val, gpu_vec + Int_val(ocaml_id),
                                sizeof(int32_t), cudaMemcpyDeviceToHost);
  if (rslt) {
    printf("Error getting element of gpu int vec: %d\n", rslt);
    exit(1);
  }
  CAMLreturn(Val_int(val));
}

/*external cuda_get_gpu_int32_vec_elt : GpuPtr.t -> int -> Int32.t
  = "ocaml_cuda_get_gpu_int32_vec_elt" 
*/ 
CAMLprim
value 
ocaml_cuda_get_gpu_int32_vec_elt(value ocaml_gpu_vec, value ocaml_id) {
  CAMLparam2(ocaml_gpu_vec, ocaml_id);
  int32_t *gpu_vec = (int*)Int32_val(ocaml_gpu_vec);
  int32_t val = 0;
  cudaError_t rslt = cudaMemcpy(&val, gpu_vec + Int_val(ocaml_id),
                                sizeof(int32_t), cudaMemcpyDeviceToHost);
  if (rslt) {
    printf("Error getting element of gpu int vec: %d\n", rslt);
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
  float *gpu_vec = (float*)Int32_val(ocaml_gpu_vec);
  float val = 0.0;
  cudaError_t rslt = cudaMemcpy(&val, gpu_vec + Int_val(ocaml_id),
                                sizeof(float), cudaMemcpyDeviceToHost);
  if (rslt) {
    printf("Error getting element of gpu int vec: %d\n", rslt);
    exit(1);
  }
  CAMLreturn(copy_double(val));
}

/* 
   Set a GPU array element 
*/ 

/*external cuda_set_gpu_int32_vec_elt : GpuPtr.t -> int -> Int32.t -> unit
*/ 
CAMLprim
void 
ocaml_cuda_set_gpu_int32_vec_elt(value ocaml_gpu_vec, value ocaml_id, 
                                     value ocaml_elt) {
                                     
  CAMLparam3(ocaml_gpu_vec, ocaml_id, ocaml_elt);
  int32_t gpu_vec = Int32_val(ocaml_gpu_vec);
  int32_t arr[1]; 
  arr[0] = Int32_val(ocaml_elt);
  CUresult result = cuMemcpyHtoD(gpu_vec, arr, sizeof(int32_t));
  if (result) {
    printf("Error setting int32 element of gpu int vec: %d\n", result);
    exit(1);
  }
  CAMLreturn0;
}

CAMLprim
void 
ocaml_cuda_set_gpu_float32_vec_elt(value ocaml_gpu_vec, value ocaml_id, 
                                     value ocaml_elt) {
  CAMLparam3(ocaml_gpu_vec, ocaml_id, ocaml_elt);
  int32_t gpu_vec = Int32_val(ocaml_gpu_vec);
  float arr[1]; 
  arr[0] = Double_val(ocaml_elt);
  CUresult result = cuMemcpyHtoD(gpu_vec, arr, sizeof(float));
  if (result) {
    printf("Error setting float32 element of gpu int vec: %d\n", result);
    exit(1);
  }
  CAMLreturn0;
}



CAMLprim
value ocaml_cuda_module_get_tex_ref(value ocaml_module_ptr, value ocaml_name) {
  CAMLparam2(ocaml_module_ptr, ocaml_name);

  CUtexref tex_ref;
  cuModuleGetTexRef(&tex_ref, *(CUmodule*)Int64_val(ocaml_module_ptr),
                    String_val(ocaml_name));

  CAMLreturn(caml_copy_int64((int64_t)tex_ref));
}

CAMLprim
value ocaml_cuda_bind_texture_2d_std_channel(
    value tex_ref, value dev_ptr, value width, value height, value kind) {
  CAMLparam5(tex_ref, dev_ptr, width, height, kind);

  unsigned int c_width = Int_val(width);
  unsigned int c_height = Int_val(height);
  int c_kind = Int_val(kind);

  // For now, all supported types are 32 bits.
  unsigned int pitch = c_kind * 4;
  CUDA_ARRAY_DESCRIPTOR desc;
  switch(kind) {
    case 0:
      desc.Format = CU_AD_FORMAT_SIGNED_INT32;
      break;
    case 1:
      desc.Format = CU_AD_FORMAT_UNSIGNED_INT32;
      break;
    case 2:
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
  cuTexRefSetAddress2D((CUtexref)Int32_val(tex_ref), &desc,
                       (CUdeviceptr)Int32_val(dev_ptr), pitch);
  
  CAMLreturn(Val_unit);
}

CAMLprim
value ocaml_unbind_texture(value tex_ref) {
  CAMLparam1(tex_ref);

  cudaUnbindTexture((textureReference*)Int32_val(tex_ref));
  
  CAMLreturn(Val_unit);
}

}
