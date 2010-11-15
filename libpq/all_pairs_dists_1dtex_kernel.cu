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

#ifdef __cplusplus
extern "C" {
#endif

texture<int, 1, cudaReadModeElementType> allDistsLeft1DTex;
texture<int, 1, cudaReadModeElementType> allDistsRight1DTex;
__global__ void all_pairs_dists_1dtex_kernel(int left_len, int right_len,
                                             int vec_len, float *output) {
  int left_id  = blockIdx.y * THREADS_PER_DIM + threadIdx.y;
  int right_id = blockIdx.x * THREADS_PER_DIM + threadIdx.x;
  if (left_id < left_len && right_id < right_len) {
    float result = 0.0f;
    float intermediate;
    int i;
    int l = left_id * vec_len;
    int r = right_id * vec_len;
    for (i = 0; i < vec_len; ++i) {
      intermediate = tex1Dfetch(allDistsLeft1DTex, l + i) -
                     tex1Dfetch(allDistsRight1DTex, r + i);
      result += intermediate * intermediate;
    }
    output[left_id * right_len + right_id] = sqrtf(result);
  }
}

#ifdef __cplusplus
}
#endif
