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

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/bigarray.h"
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/callback.h"
#include "caml/custom.h"

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

/* Int64.t -> Int32.t -> int -> unit */
CAMLprim
value ocaml_cuda_memcpy_to_device (value array,
                    value dev_ptr,
                    value num_bytes) {
  CAMLparam3(array, dev_ptr, num_bytes);
  void* source = (void*)Int64_val(array);
  CUdeviceptr dest = Int32_val(dev_ptr);
 // printf("in memcpy to device, source[0]: %d\n", ((int*)source)[0]);
  CUresult result = cuMemcpyHtoD(dest, source, Int_val(num_bytes));
  if (result != 0) {
    printf ("Error copying memory from host to GPU: %d \n", result);
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
  } //else {
    // printf("memcpy to host succeeded, dest[0]: %d\n", ((int*)dest)[0]);
  //}
  CAMLreturn(Val_unit);
}
