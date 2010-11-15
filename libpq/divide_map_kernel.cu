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

__global__
void divide_map_kernel(int div_by, int vec_len, float *output) {
  int id = blockIdx.y * 16384 * THREADS_PER_BLOCK +
           blockIdx.x * THREADS_PER_BLOCK + threadIdx.x;
  if (id < vec_len) {
    output[id] = output[id] / div_by;
  }
}

#ifdef __cplusplus
}
#endif
