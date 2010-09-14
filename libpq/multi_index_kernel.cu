/*
 * Parallel Q Library
 *
 * (c) 2009-2010 Eric Hielscher
 *
 * Perform the first pass of multi-indexing.
 */

#include <cuda.h>
#include <stdio.h>

#define THREADS_PER_BLOCK 512

/*
 * Takes as input:
 *
 * - A vector of input vectors.
 * - A matrix of validities.
 *
 * For each column in the validities matrix, computes the average of the els
 * with 1s.
 *
 * The grid is expected to have one warp per element in the validities matrix.
 *
 * The output is equal to a matrix with:
 * - The number of rows equal to the number of centroids.
 * - A number of vectors equal to num_blocks, with each vector being vec_len.
 */

__global__ void
multi_index_compact_kernel(float *input, int *valids, int k, float *output) {
  // For now, just allocate the maximum amount of shared memory.
  // This won't support vectors of length longer than 32, and is quite
  // inefficient for smaller vectors due to not being able to schedule
  // the maximum number of warps per SM.
  __shared__ float cache[512];

  int tid    = threadIdx.x;
  int warpId = tid / 32;
  int valid;

  // Each warp needs to either bring in its vector, if the vector is valid,
  // or no-op here.
  valid = valids[k * (blockIdx.y * 16 + warpId) + blockIdx.x];
  if (valid) {
    cache[tid] = input[512 * blockIdx.y + tid];
  } else {
    cache[tid] = 0.0f;
  }
  __syncthreads();
  
  // Now perform the intra-block unrolled parallel reduction.
  if (tid < 256) {
    cache[tid] += cache[tid + 256];
  }
  __syncthreads();

  if (tid < 128) {
    cache[tid] += cache[tid + 128];
  }
  __syncthreads();
  
  if (tid < 64) {
    cache[tid] += cache[tid + 64];
  }
  __syncthreads();
  
  if (tid < 32) {
    output[(gridDim.x / 16 * blockIdx.x + blockIdx.y) * 32 + tid] =
      cache[tid] + cache[tid + 32];
  }
}

__global__ void
multi_index_reduce_kernel(float *input, float *output, int k) {
  __shared__ float cache[256];
  
  int tid = threadIdx.x;
  int offset = (blockIdx.y * gridDim.x + blockIdx.x) * 512;
  
  // Load the shared memory cache.
  float left  = input[offset + tid];
  float right = input[offset + tid + 256];
  cache[tid] = left + right;
  __syncthreads();
  
  // Reduce.  
  if (tid < 128) {
    cache[tid] += cache[tid + 128];
  }
  __syncthreads();
  
  if (tid < 64) {
    cache[tid] += cache[tid + 64];
  }
  __syncthreads();
  
  if (tid < 32) {
    output[(blockIdx.y * gridDim.x + blockIdx.x) * 32 + tid] =
      cache[tid] + cache[tid + 32];
  }
}

__global__ void
reduce_valids_first_kernel(int *input, int *output,
                           int num_centroids)
{
    __shared__ int cache[256];

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid      = threadIdx.x;
    unsigned int offset   =
      (blockIdx.y * num_centroids + tid) * 512 + blockIdx.x;

    int left  = input[offset];
    int right = input[offset + num_centroids * 256];
    cache[tid] = left + right;
    __syncthreads();

    // do reduction in shared mem
    if (tid < 128) { cache[tid] += cache[tid + 128]; } __syncthreads();
    if (tid <  64) { cache[tid] += cache[tid +  64]; } __syncthreads();
    
    if (tid < 32)
    {
        cache[tid] += cache[tid + 32];
        cache[tid] += cache[tid + 16];
        cache[tid] += cache[tid +  8];
        cache[tid] += cache[tid +  4];
        cache[tid] += cache[tid +  2];
        cache[tid] += cache[tid +  1];
    }
    
    // write result for this block to global mem 
    if (tid == 0) output[num_centroids * blockIdx.x + blockIdx.y] = cache[0];
}

__global__ void
reduce_valids_recursive_kernel(int *input, int *output,
                               int num_centroids)
{
    __shared__ int cache[256];

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid      = threadIdx.x;
    unsigned int offset   = (gridDim.x + blockIdx.x) * 512;

    int left  = input[offset];
    int right = input[offset + 256];
    cache[tid] = left + right;
    __syncthreads();

    // do reduction in shared mem
    if (tid < 128) { cache[tid] += cache[tid + 128]; } __syncthreads();
    if (tid <  64) { cache[tid] += cache[tid +  64]; } __syncthreads();
    
    if (tid < 32)
    {
        cache[tid] += cache[tid + 32];
        cache[tid] += cache[tid + 16];
        cache[tid] += cache[tid +  8];
        cache[tid] += cache[tid +  4];
        cache[tid] += cache[tid +  2];
        cache[tid] += cache[tid +  1];
    }
    
    // write result for this block to global mem 
    if (tid == 0) output[num_centroids * gridDim.x + blockIdx.x] = cache[0];
}

__global__ void
final_division_kernel(float *sums, int *counts)
{
  int tid = threadIdx.x;
  int idx = blockIdx.x * 256 + tid;
  int count = counts[blockIdx.x * 256 + tid / 32];
  float element = sums[idx];
  sums[idx] = element / count;
}

