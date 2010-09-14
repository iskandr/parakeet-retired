/*
 * Parallel Q Library
 *
 * (c) 2009-2010 Eric Hielscher
 *
 * Min all of a vector of vectors of floats.
 */

#include <cuda.h>
#include <stdio.h>

#define THREADS_PER_BLOCK 256

/*
 * Assumes that the size of the output array is equal to the number of thread
 * blocks.
 *
 * Takes an x by y list of floats and produces a y-element list of floats
 * where the nth element is the x-index in the original matrix of the minimum
 * element in the yth row.
 *
 * Assumes that the output array has been allocated to be y floats long.
 *
 * Each block processes THREADS_PER_BLOCK * 2 elements from a row at a time.
 *
 * If the x-dimension of the input is longer than that, we follow the reduce
 * pattern, where we find the minimum for the blocks, write the partial results
 * to a y-element by num_blocks_element intermediate, and then recursively call
 * the kernels.
 *
 * The following kernel, for now, only works if the x_len is equal to
 * THREADS_PER_BLOCK * 2.
 */

__global__ void
minall_kernel(float *input, int *output, int x_len, int y_len)
{
    __shared__ float cache[THREADS_PER_BLOCK];
    __shared__ int   idx[THREADS_PER_BLOCK];

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    int tid      = threadIdx.x;
    int i        = blockIdx.x * x_len + tid;

    // Initialize the output to all zeros.
    output[i] = 0;
    output[i + THREADS_PER_BLOCK] = 0;

    float left = input[i];
    float right = input[i + THREADS_PER_BLOCK];
    if (left > right) {
      cache[tid] = right;
      idx[tid] = tid + THREADS_PER_BLOCK;
    } else {
      cache[tid] = left;
      idx[tid] = tid;
    }
    __syncthreads();
    
    if (tid < 128) {
      if (cache[tid] > cache[tid + 128]) {
        cache[tid] = cache[tid + 128];
        idx[tid] = idx[tid + 128];
      }
    }
    __syncthreads();
    
    if (tid < 64) {
      if (cache[tid] > cache[tid + 64]) {
        cache[tid] = cache[tid + 64];
        idx[tid] = idx[tid + 64];
      }
    }
    __syncthreads();
    
    if (tid < 32)
    {
      if (cache[tid] > cache[tid + 32]) {
        cache[tid] = cache[tid + 32];
        idx[tid] = idx[tid + 32];
      }
      if (cache[tid] > cache[tid + 16]) {
        cache[tid] = cache[tid + 16];
        idx[tid] = idx[tid + 16];
      }
      if (cache[tid] > cache[tid + 8]) {
        cache[tid] = cache[tid +  8];
        idx[tid] = idx[tid + 8];
      }
      if (cache[tid] > cache[tid + 4]) {
        cache[tid] = cache[tid +  4];
        idx[tid] = idx[tid + 4];
      }
      if (cache[tid] > cache[tid + 2]) {
        cache[tid] = cache[tid +  2];
        idx[tid] = idx[tid + 2];
      }
      if (cache[tid] > cache[tid + 1]) {
        cache[tid] = cache[tid +  1];
        idx[tid] = idx[tid + 1];
      }
    }
    
    // write result for this block to global mem
    if (tid == 0) {
      output[blockIdx.x * x_len + idx[0]] = 1;
    }
}

