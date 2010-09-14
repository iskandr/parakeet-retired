/*
 * Parallel Q Library
 *
 * (c) 2009-2010 Eric Hielscher
 *
 * Reduction.
 */

#include <cuda.h>
#include <stdio.h>

#define THREADS_PER_BLOCK 256

/*
 * Assumes that the size of the output array is equal to the number of thread
 * blocks.
 */

__global__ void
reduce_float_kernel(float *input, float *output, unsigned int num_input)
{
    __shared__ float cache[THREADS_PER_BLOCK];

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid      = threadIdx.x;
    unsigned int i        = blockIdx.x*(THREADS_PER_BLOCK*2) + threadIdx.x;
    unsigned int gridSize = THREADS_PER_BLOCK*2*gridDim.x;
    cache[tid] = 0;

    // we reduce multiple elements per thread.  The number is determined by the 
    // number of active thread blocks (via gridDim).  More blocks will result
    // in a larger gridSize and therefore fewer elements per thread
    while (i < num_input)
    {         
        cache[tid] += input[i];
        // ensure we don't read out of bounds
        // Note: Can remove the if when n is a power of 2
        if (i + THREADS_PER_BLOCK < num_input) 
            cache[tid] += input[i + THREADS_PER_BLOCK];  
        i += gridSize;
    } 
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
    if (tid == 0) output[blockIdx.x] = cache[0];
}

__global__ void
reduce_int_kernel(int *input, int *output, unsigned int num_input)
{
    __shared__ int cache[THREADS_PER_BLOCK];

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid      = threadIdx.x;
    unsigned int i        = blockIdx.x*(THREADS_PER_BLOCK*2) + threadIdx.x;
    unsigned int gridSize = THREADS_PER_BLOCK*2*gridDim.x;
    cache[tid] = 0;

    // we reduce multiple elements per thread.  The number is determined by the 
    // number of active thread blocks (via gridDim).  More blocks will result
    // in a larger gridSize and therefore fewer elements per thread
    while (i < num_input)
    {         
        cache[tid] += input[i];
        // ensure we don't read out of bounds
        // Note: Can remove the if when n is a power of 2
        if (i + THREADS_PER_BLOCK < num_input) 
            cache[tid] += input[i + THREADS_PER_BLOCK];  
        i += gridSize;
    } 
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
    if (tid == 0) output[blockIdx.x] = cache[0];
}

