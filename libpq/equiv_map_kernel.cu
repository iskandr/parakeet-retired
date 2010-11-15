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

texture<float, 1, cudaReadModeElementType> findMinInputTex;
__global__
void equiv_map_kernel(int input_len, int *equiv) {
  int id = blockIdx.y * 16384 * THREADS_PER_BLOCK +
           blockIdx.x * THREADS_PER_BLOCK + threadIdx.x;
  if (id < num_rows) {
    int idx = id * vec_len;
    int i;
    float tmp;
    float min = tex1Dfetch(findMinInputTex, idx);
    int minidx = 0;
    for (i = 1; i < vec_len; ++i) {
      tmp = tex1Dfetch(findMinInputTex, idx + i);
      if (tmp < min) {
        min = tmp;
        minidx = i;
      }
    }
    output[id] = minidx;
  }
}

#ifdef __cplusplus
}
#endif
