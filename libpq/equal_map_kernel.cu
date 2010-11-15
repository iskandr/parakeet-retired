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

texture<int, 1, cudaReadModeElementType> equalMapInputTex;
__global__
void equal_map_kernel(int test_val, int input_len, int *output) {
  int id = blockIdx.y * 16384 * THREADS_PER_BLOCK +
           blockIdx.x * THREADS_PER_BLOCK + threadIdx.x;
  if (id < input_len) {
    int in = tex1Dfetch(equalMapInputTex, id);
    if (in == test_val) {
      output[id] = 1;
    } else {
      output[id] = 0;
    }
  }
}

#ifdef __cplusplus
}
#endif
