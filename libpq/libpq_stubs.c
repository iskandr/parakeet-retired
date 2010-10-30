// OCaml interface to PQ library functions

#include <stdint.h>
#include <stdio.h>
#include <time.h>

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/bigarray.h"
#include "caml/alloc.h"
#include "caml/fail.h"
#include "caml/callback.h"
#include "caml/custom.h"

#include "../OCAMLInterface/variants.h"
#include "string.h"
#include "pqlib.h"

#define ALIGN_UP(offset, alignment) \
  (offset) = ((offset) + (alignment) - 1) & ~((alignment) - 1)


/* unit -> unit */
CAMLprim value ocaml_pq_init(value unit) {
  CAMLparam1 (unit);
  if (cuInit(0) != CUDA_SUCCESS) {
    exit (0);
  }
  CAMLreturn (Val_unit);
}

static const int jitLogBufferSize = 32000;

/* string -> Int64.t */
CAMLprim
value ocaml_pq_compile_module(value ptx_string, value threads_per_block)
{
  CAMLparam2(ptx_string, threads_per_block);
  CUmodule *cuModule = (CUmodule*)(malloc(sizeof(CUmodule)));

  // in this branch we use compilation with parameters
  const unsigned int jitNumOptions = 6;
  CUjit_option jitOptions[] =  {
      CU_JIT_INFO_LOG_BUFFER_SIZE_BYTES,
      CU_JIT_INFO_LOG_BUFFER,
      CU_JIT_ERROR_LOG_BUFFER_SIZE_BYTES,
      CU_JIT_ERROR_LOG_BUFFER, 
      CU_JIT_THREADS_PER_BLOCK,
      CU_JIT_WALL_TIME
  };

  char ebuf[jitLogBufferSize];
  char ibuf[jitLogBufferSize];
  int nthreads = Int_val(threads_per_block); 

  void *jitOptVals[] = {
      (void*)jitLogBufferSize,
      ebuf,
      (void*)jitLogBufferSize,
      ibuf,
      (void*) nthreads,
      (void*) 0, /* since wall time is an output only variable, don't need anything here */ 
  };

  CUresult result = cuModuleLoadDataEx(cuModule, String_val(ptx_string),
	     jitNumOptions, jitOptions, jitOptVals);

  printf("JIT info log: %s\n", ibuf);
  if (result != 0) {
    printf("Error #%d compiling module %p \n", result, cuModule);

	  printf("JIT error log: %s\n", ebuf);
	exit(1);
#ifdef DEBUG
  } else {
    printf("JIT max threads per block: %d (requested: %d)\n",
           (int) jitOptVals[4], nthreads);
    float jitTime = 0.0; 
    memcpy((void*) &jitTime, &jitOptVals[5], sizeof(float));
    printf("JIT compile time: %f\n", jitTime);
#endif
  }
  CAMLreturn(caml_copy_int64((int64_t)cuModule));
}

/* Int64.t -> unit */
CAMLprim value ocaml_pq_destroy_module(value module_ptr) {
	CAMLparam1(module_ptr);

  CUmodule *cuModule = (CUmodule*)Int64_val(module_ptr);
  CUresult result =  cuModuleUnload(*cuModule);
  if (result != 0) {
    printf("Error #%d unloading module %p \n", result, cuModule);
    free(cuModule);
    exit(1);
  }

  free(cuModule);
	CAMLreturn(Val_unit);
}




CAMLprim value ocaml_pq_launch_ptx (
  value ocaml_module_ptr,
  value ocaml_ptx_fun,
  value ocaml_args,
  value ocaml_threadsx,
  value ocaml_threadsy,
  value ocaml_threadsz,
  value ocaml_gridwidth,
  value ocaml_gridheight) {

  CAMLparam5(ocaml_module_ptr, ocaml_ptx_fun, ocaml_args,
             ocaml_threadsx, ocaml_threadsy);
  CAMLxparam3(ocaml_threadsz, ocaml_gridwidth, ocaml_gridheight);
  CAMLlocal2(ocaml_gpu_arg, ocaml_gpu_val);

  int threadsx = Int_val(ocaml_threadsx); 
  int threadsy = Int_val(ocaml_threadsy); 
  int threadsz = Int_val(ocaml_threadsz); 

#ifdef DEBUG
  printf("Block dimensions: x: %d y: %d z: %d\n",threadsx, threadsy, threadsz);

  printf("Grid dimensions: x: %d, y: %d \n", Int_val(ocaml_gridwidth),
           Int_val(ocaml_gridheight));
#endif

  // Have to take the args array and pull out a list of the arguments,
  // including inserting shape vectors for those arguments which need them.
  //
  // Also, I allocate twice the number of args for these arrays, because each
  // non-scalar arg results in two actual GPU args.  Slight amount of wasted
  // space for scalars but whatever.
  CUmodule *cuModule = (CUmodule*)Int64_val(ocaml_module_ptr);
  CUfunction cuFunc;
  CUresult result;

    // Get the function and set its block shape.
  char* fnName = String_val(ocaml_ptx_fun);
  result = cuModuleGetFunction(&cuFunc, *cuModule, fnName);
  if (result != 0) {
    printf("cuModuleGetFunction failed for %s with error %d", fnName, result);
    exit(1);
  }
#ifdef DEBUG
  else { printf("Got function %s from module\n", fnName); }
#endif

  result = cuFuncSetBlockShape(cuFunc, threadsx, threadsy, threadsz); 
  if (result != 0) {
    printf("Error in cuFuncSetBlockShape: %d\n", result);
  }


  int num_args = Wosize_val(ocaml_args);
  void *ptr_arg = NULL;
  int offset = 0;
  int arg_size = 0;
  int i;

#ifdef DEBUG
  printf("Setting up %d GPU arguments\n", num_args);
#endif
  for (i = 0; i < num_args; ++i) {


    ocaml_gpu_arg = Field(ocaml_args, i);

    if (Tag_val(ocaml_gpu_arg) == GpuArray) {
      printf("Sending array to GPU!\n");
      ocaml_gpu_val = Field(ocaml_gpu_arg, 0);
      // First, we just add the arg to the list of args
      ptr_arg = (void*)Int32_val(Field(ocaml_gpu_val, GpuArray_VEC_PTR));
      ALIGN_UP(offset, sizeof(void*));
      result = cuParamSetv(cuFunc, offset, &ptr_arg, sizeof(void*));
      if (result != 0) {
        printf("Error #%d in cuParamSetv, arg %d of %d (GpuArray data)",
            result,  i, num_args);
        exit(1);
      }
      offset += sizeof(void*);

            // Now, add the shape vector too
      ptr_arg = (void*)Int32_val(Field(ocaml_gpu_val, GpuArray_VEC_SHAPE_PTR));
      ALIGN_UP(offset, sizeof(void*));
      result = cuParamSetv(cuFunc, offset, &ptr_arg, sizeof(void*));
      if (result != 0) {
        printf("Error #%d in cuParamSetv, arg %d of %d (GpuArray shape)",
            result, i, num_args);
        exit(1);
      }
      offset += sizeof(void*);

    } else if (Tag_val(ocaml_gpu_arg) == GpuScalar) {
      ocaml_gpu_val = Field(ocaml_gpu_arg, 0);
      int pqnum_tag = Tag_val(ocaml_gpu_val);
      /* locals used to pull out number values */
      int32_t int32_val;
      int64_t int64_val;
      union { int32_t fbits; float f; } f32_union = { 0 };
      union { int64_t dbits; double d; } f64_union = { 0 };

      switch (pqnum_tag) {

      case PQNUM_INT32:
        int32_val = get_pqnum_int32(ocaml_gpu_val);
        ptr_arg = (void*) &int32_val;
        arg_size = sizeof(int32_t);
        break;

      case PQNUM_INT64:
        int64_val = get_pqnum_int64(ocaml_gpu_val);
        ptr_arg = (void*) &int64_val;
        arg_size = sizeof(int64_t);
        break;

      case PQNUM_FLOAT32:
        /* get the bits of a float32 */
        f32_union.f =  get_pqnum_float32(ocaml_gpu_val);
        printf("Got PQNUM.f32: %f\n", f32_union.f);
        ptr_arg = (void*) &(f32_union.fbits);
        arg_size = sizeof(float);
        break;

      case PQNUM_FLOAT64:
        /* get the bits of a float64 */
        f64_union.d = get_pqnum_float64(ocaml_gpu_val);
        printf("Got PQNUM.f64: %f\n", f64_union.d);
        ptr_arg = (void*) &(f64_union.dbits);
        arg_size = sizeof(double);
        break;

      default:
        printf("Scalars of type tag %d not yet supported by ocaml_launch_ptx\n",
               pqnum_tag);
        exit(1);

      }
      printf("Transmitting scalar to GPU (address: %p, size: %d)", ptr_arg, arg_size);
      ALIGN_UP(offset, arg_size);
      result = cuParamSetv(cuFunc, offset, ptr_arg, arg_size);
      if (result != 0) {
        printf("Error #%d in cuParamSetv -- GpuScalar of %d bytes", result, arg_size);
        exit(1);
      }
      offset += arg_size;

    } else {
      printf("Unrecognized GPU argument type for function %s. Aborting.\n",
             fnName);
      exit(1);
    }
  }

  result = cuParamSetSize(cuFunc, offset);
  if (result != 0) {
    printf ("Error in cuParamSetSize: %d\n", result);
    exit(1);
  }
  int width = Int_val(ocaml_gridwidth);
  int height = Int_val(ocaml_gridheight);

#ifdef DEBUG
  printf("Grid width, height: %d, %d\n", width, height);
  fflush(stdout);
#endif

  result = cuLaunchGrid(cuFunc, width, height);
  if (result != 0) {
    printf("Error launching grid: %d\n", result);
    exit(1);
  }
  result = cuCtxSynchronize();

  if (result != 0) {
    printf("Error during kernel: %d\n", result);
    exit(1);
  }

  CAMLreturn(Val_unit);
}

value ocaml_pq_launch_ptx_bytecode (value *argv, int argn) {
  return ocaml_pq_launch_ptx(argv[0], argv[1], argv[2], argv[3],
                             argv[4], argv[5], argv[6], argv[7]);
}

CAMLprim value ocaml_cuda_memcpy_to_constant(value data, value bytes,
                                             value module, value symbol) {
  CAMLparam4(data, bytes, module, symbol);
  
  CAMLreturn(Val_unit);
}
