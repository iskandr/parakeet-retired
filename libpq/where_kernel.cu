/*
 * Parallel Q Library
 *
 * (c) 2009-2010 Eric Hielscher
 *
 * Each-left each-right implementations.
 */

#include <cuda.h>
#include <stdio.h>

#include "base.h"

#define THREADS_PER_BLOCK 256

#ifdef __cplusplus
extern "C" {
#endif

texture<int, 1, cudaReadModeElementType> whereInputTex;
__global__
void where_kernel(int num_input, int *output) {
  int tid = threadIdx.x;
  int id = blockIdx.y * 16384 * THREADS_PER_BLOCK +
           blockIdx.x * THREADS_PER_BLOCK + tid;

  int prev = 0;
  if (id < num_input) {
    if (id > 0) {
      prev = tex1Dfetch(whereInputTex, id-1);
    }

    if (prev != tex1Dfetch(whereInputTex, id)) {
      output[prev] = id;
    }
  }
}

#ifdef __cplusplus
}
#endif
