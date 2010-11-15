/*
 * Parallel Q Library
 *
 * (c) 2009-2010 Eric Hielscher
 *
 * Reduction.
 */

#include <cuda.h>
#include <stdio.h>

#define THREADS_PER_BLOCK 128

/*
 * Assumes that the size of the output array is equal to the number of thread
 * blocks.
 */

int safe_div(int n, int d) {
  return (n + d - 1) / d;
}

__global__ void
find_int_kernel(int *input, int *output, int num_input, int val) {
  __shared__ int cache[THREADS_PER_BLOCK];

  unsigned int tid            = threadIdx.x;
  unsigned int linearBlockIdx = blockIdx.x + (blockIdx.y * gridDim.x);
  unsigned int startBlock     = THREADS_PER_BLOCK * 2 * linearBlockIdx;
  unsigned int i              = startBlock + tid;

  cache[tid] = -1;
  if (i < num_input) {
    if (input[i] == val) {
      cache[tid] = i;
    } else if (i + THREADS_PER_BLOCK < num_input) {
      cache[tid] = i + THREADS_PER_BLOCK;
    }
  }
  __syncthreads();

  if (tid < 64) {
    if (cache[tid] == -1 && cache[tid + 64] != -1) {
      cache[tid] = cache[tid + 64];
    }
  } __syncthreads();

  if (tid < 32) {
    if (cache[tid] == -1 && cache[tid + 32] != -1) {
      cache[tid] = cache[tid + 32];
    }
    if (cache[tid] == -1 && cache[tid + 16] != -1) {
      cache[tid] = cache[tid + 16];
    }
    if (cache[tid] == -1 && cache[tid +  8] != -1) {
      cache[tid] = cache[tid +  8];
    }
    if (cache[tid] == -1 && cache[tid +  4] != -1) {
      cache[tid] = cache[tid +  4];
    }
    if (cache[tid] == -1 && cache[tid +  2] != -1) {
      cache[tid] = cache[tid +  2];
    }
    if (cache[tid] == -1 && cache[tid +  1] != -1) {
      cache[tid] = cache[tid +  1];
    }
  }

  if (tid == 0) output[linearBlockIdx] = cache[0];
}
