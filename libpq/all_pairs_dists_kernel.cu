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
#define THREADS_PER_DIM 16

#ifdef __cplusplus
extern "C" {
#endif

texture<int, 2, cudaReadModeElementType> allDistsLeft2DTex;
texture<int, 2, cudaReadModeElementType> allDistsRight2DTex;
__global__ void all_pairs_dists_kernel(int left_len, int right_len,
                                       int vec_len, float *output) {
  int left_id  = blockIdx.y * THREADS_PER_DIM + threadIdx.y;
  int right_id = blockIdx.x * THREADS_PER_DIM + threadIdx.x;
  if (left_id < left_len && right_id < right_len) {
    float result = 0.0f;
    float intermediate;
    int i;
    for (i = 0; i < vec_len; ++i) {
      intermediate = tex2D(allDistsLeft2DTex, i, left_id) -
                     tex2D(allDistsRight2DTex, i, right_id);
      result += intermediate * intermediate;
    }
    output[left_id * right_len + right_id] = sqrtf(result);
  }
}

#ifdef __cplusplus
}
#endif
