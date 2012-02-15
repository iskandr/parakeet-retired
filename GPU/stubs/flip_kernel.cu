/*
 * Parallel Q Library
 *
 * (c) 2009-2011 Eric Hielscher
 *
 * Flip implementation.
 */

#include <cuda.h>
#include <stdio.h>

#include "base.h"

#define THREADS_PER_BLOCK 256

#ifdef __cplusplus
extern "C" {
#endif

/** TODO: Can probably be more efficient with, say, many long vectors by
 *        having one thread per element (say, a linear block per vector)
 *        than by having one thread per vector as done here and in Rodinia **/

__global__
void flip_int_2D_kernel(int *input, int in_height, int in_width, int *output) {
  int tid = threadIdx.x;
  int row_id = blockIdx.y * 16384 * THREADS_PER_BLOCK +
               blockIdx.x * THREADS_PER_BLOCK + tid;

  int i;

  if (row_id < in_height) {
    for (i = 0; i < in_width; ++i) {
      output[row_id + in_height * i] = input[row_id * in_width + i];
    }
  }
}

__global__
void flip_float_2D_kernel(float *input, int in_height, int in_width,
                          float *output) {
  int tid = threadIdx.x;
  int row_id = blockIdx.y * 16384 * THREADS_PER_BLOCK +
               blockIdx.x * THREADS_PER_BLOCK + tid;

  int i;

  if (row_id < in_height) {
    for (i = 0; i < in_width; ++i) {
      output[row_id + in_height * i] = input[row_id * in_width + i];
    }
  }
}

#ifdef __cplusplus
}
#endif
