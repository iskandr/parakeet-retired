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

#include "string.h"
#include "pqlib.h"

#define ALIGN_UP(offset, alignment) \
  (offset) = ((offset) + (alignment) - 1) & ~((alignment) - 1)

/**
  * Enums for the Variant Types I use - needs to be updated if the types
  * change!
  *
  * I put the non-data Variants first for each type, then the data versions
 **/
enum gpu_val_data {
  GpuVec = 0,
  GpuScalar
};

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
  CAMLlocal3(ocaml_arg, ocaml_gpu_val, ocaml_gpu_var);

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

    ocaml_gpu_val = Field(ocaml_args, i);
    ocaml_gpu_var = Field(ocaml_gpu_val, 0);
    if (Tag_val(ocaml_gpu_val) == GpuVec) {
      // First, we just add the arg to the list of args
      ptr_arg = (void*)Int32_val(Field(ocaml_gpu_var, 0));
      ALIGN_UP(offset, sizeof(void*));
      result = cuParamSetv(cuFunc, offset, &ptr_arg, sizeof(void*));
      if (result != 0) {
        printf("Error #%d in cuParamSetv, arg %d of %d (GpuVec data)",
            result,  i, num_args);
        exit(1);
      }
      offset += sizeof(void*);

            // Now, add the shape vector too
      ptr_arg = (void*)Int32_val(Field(ocaml_gpu_var, 3));
      ALIGN_UP(offset, sizeof(void*));
      result = cuParamSetv(cuFunc, offset, &ptr_arg, sizeof(void*));
      if (result != 0) {
        printf("Error #%d in cuParamSetv, arg %d of %d (GpuVec shape)",
            result, i, num_args);
        exit(1);
      }
      offset += sizeof(void*);

    } else if (Tag_val(ocaml_gpu_val) == GpuScalar) {
      
      // GpuScalar
      arg_size = Int_val(Field(ocaml_gpu_var, 2));
      ptr_arg = (void*)Int64_val(Field(ocaml_gpu_var, 0));
      ALIGN_UP(offset, arg_size);
      result = cuParamSetv(cuFunc, offset, &ptr_arg, arg_size);
      if (result != 0) {
        printf("Error #%d in cuParamSetv, arg %d of %d (GpuScalar of %d bytes)",
            result, arg_size);
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
