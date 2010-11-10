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

/*
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
*/

texture<int, 1, cudaReadModeElementType> reduceIntVecsTex;
__global__ void
reduce_int_kernel_nonopt(int *output, unsigned int num_input)
{
    __shared__ int cache[THREADS_PER_BLOCK];

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid            = threadIdx.x;
    unsigned int linearBlockIdx = blockIdx.x + (blockIdx.y * gridDim.x);
    unsigned int startBlock     = THREADS_PER_BLOCK * 2 * linearBlockIdx;
    unsigned int i              = startBlock + threadIdx.x;

    if (i < num_input) {
      if ((i + THREADS_PER_BLOCK) < num_input) {
        int tmp1 = tex1Dfetch(reduceIntVecsTex, i);
        int tmp2 = tex1Dfetch(reduceIntVecsTex, i + THREADS_PER_BLOCK);
        cache[tid] = tmp1 + tmp2;
      } else {
        cache[tid] = tex1Dfetch(reduceIntVecsTex, i);
      }
    }
    __syncthreads();

    unsigned int lenBlock = THREADS_PER_BLOCK * 2;
    if ((num_input - startBlock) < lenBlock) {
      lenBlock = num_input - startBlock;
    }

    // do reduction in shared mem
    if (tid < 64 && (tid + 64) < lenBlock) {
      cache[tid] += cache[tid + 64];
    } __syncthreads();
    if (tid < 32 && (tid + 32) < lenBlock) {
      cache[tid] += cache[tid + 32];
    } __syncthreads();
    if (tid < 16 && (tid + 16) < lenBlock) {
      cache[tid] += cache[tid + 16];
    } __syncthreads();
    if (tid <  8 && (tid +  8) < lenBlock) {
      cache[tid] += cache[tid +  8];
    } __syncthreads();
    if (tid <  4 && (tid +  4) < lenBlock) {
      cache[tid] += cache[tid +  4];
    } __syncthreads();
    if (tid <  2 && (tid +  2) < lenBlock) {
      cache[tid] += cache[tid +  2];
    } __syncthreads();
    if (tid == 0 && lenBlock > 1) {
      output[linearBlockIdx] = cache[0] + cache[1];
    }
}

#ifdef __cplusplus
extern "C" {
#endif

void reduce(int *input, int* output, int num_input) {
  // Call min-all
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  // Alloc device copies
  int *devInput;
  int *devOutput;
  cudaEventRecord(start, 0);
  rslt = cudaMalloc((void**)&devInput, num_input * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Input: %d\n", rslt);
  // Copy data to device
  rslt = cudaMemcpy(devInput, input, num_input * sizeof(int),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy input: %d\n", rslt);
  cudaChannelFormatDesc inputDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  reduceIntVecsTex.normalized = 0;
  
  int curNum = num_input;
  while (curNum > 1) {
    int numBlocks = safe_div(curNum, THREADS_PER_BLOCK * 2);
    rslt = cudaMalloc((void**)&devOutput, numBlocks * sizeof(int));
    if (rslt != 0) printf("failed to malloc dev Output: %d\n", rslt);

    cudaBindTexture(0, reduceIntVecsTex, devInput, inputDesc,
                    curNum * sizeof(int));
    // Invoke kernel
    dim3 dimBlock(THREADS_PER_BLOCK);
    int gridX, gridY;
    if (numBlocks < 16384) {
      gridX = numBlocks;
      gridY = 1;
    } else {
      gridX = 16384;
      gridY = safe_div(numBlocks, 16384);
    }
    dim3 dimGrid(gridX, gridY);
    printf("grid.x, grid.y: %d %d\n", dimGrid.x, dimGrid.y);
    reduce_int_kernel_nonopt<<<dimGrid, dimBlock>>>
      (devOutput, curNum);
    cudaUnbindTexture(reduceIntVecsTex);
    
    curNum = numBlocks;
    cudaFree(devInput);
    devInput = devOutput;
  }

  rslt = cudaMemcpy(output, devOutput, sizeof(int),
                    cudaMemcpyDeviceToHost);
  if (rslt != 0) printf("failed to copy output to host: %d\n", rslt);

  cudaEventRecord(stop, 0);
  cudaEventSynchronize(stop);

  float t;
  cudaEventElapsedTime(&t, start, stop);
  printf("Time for kernel: %f\n", t);
  cudaEventDestroy(start);
  cudaEventDestroy(stop);

  cudaFree(devOutput);
}

#ifdef __cplusplus
}
#endif
