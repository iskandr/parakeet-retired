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

texture<int, 1, cudaReadModeElementType> inputTex;
__global__
void gen_index_kernel(int num_input, int *output) {
  int tid = threadIdx.x;
  int id = blockIdx.y * 16384 * THREADS_PER_BLOCK +
           blockIdx.x * THREADS_PER_BLOCK + tid;

  int prev = 0;
  if (id < num_input) {
    if (id > 0) {
      prev = tex1Dfetch(inputTex, id-1);
    }
    
    if (prev != tex1Dfetch(inputTex, id)) {
      output[prev] = id;
    }
  }
}

void launch_gen_index(int *input, int num_input, int *output) {
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  //Invoke kernel
  dim3 dimBlock(THREADS_PER_BLOCK);
  int num_blocks = safe_div(num_input, THREADS_PER_BLOCK);
  int gridX, gridY;
  if (num_blocks > 16384) {
    gridX = 16384;
    gridY = safe_div(num_blocks, 16384);
  } else {
    gridX = num_blocks;
    gridY = 1;
  }
  dim3 dimGrid(gridX, gridY);
  printf("grid.x, grid.y: %d %d\n", dimGrid.x, dimGrid.y);

  // Alloc device copies
  int *devInput;
  int *devOutput;
  rslt = cudaMalloc((void**)&devInput, num_input * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Input: %d\n", rslt);
  // Copy data to device
  rslt = cudaMemcpy(devInput, input, num_input * sizeof(int),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy input: %d\n", rslt);
  rslt = cudaMalloc((void**)&devOutput, input[num_input-1] * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Output: %d\n", rslt);

  cudaChannelFormatDesc inputDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  inputTex.normalized = 0;
  cudaBindTexture(0, inputTex, devInput, inputDesc, num_input * sizeof(int));

  cudaEventRecord(start, 0);
  gen_index_kernel<<<dimGrid, dimBlock>>> (num_input, devOutput);

  cudaEventRecord(stop, 0);
  cudaEventSynchronize(stop);

  rslt = cudaMemcpy(output, devOutput, sizeof(int) * input[num_input-1],
                    cudaMemcpyDeviceToHost);
  if (rslt != 0) printf("failed to copy output to host: %d\n", rslt);

  float t;
  cudaEventElapsedTime(&t, start, stop);
  printf("Time for kernel: %f\n", t);
  cudaEventDestroy(start);
  cudaEventDestroy(stop);

  cudaFree(devInput);
  cudaFree(devOutput);
}

void launch_gen_index_dev(int *input, int num_input, int *output,
                          int memtime) {
  cudaEvent_t start, stop;
  float t;
  if (memtime) {
    cudaEventCreate(&start);
    cudaEventCreate(&stop);
    cudaEventRecord(start, 0);
    cudaEventSynchronize(start);
  }
  
  //Invoke kernel
  dim3 dimBlock(THREADS_PER_BLOCK);
  int num_blocks = safe_div(num_input, THREADS_PER_BLOCK);
  int gridX, gridY;
  if (num_blocks > 16384) {
    gridX = 16384;
    gridY = safe_div(num_blocks, 16384);
  } else {
    gridX = num_blocks;
    gridY = 1;
  }
  dim3 dimGrid(gridX, gridY);
  printf("[gen_idx] grid.x, grid.y: %d %d\n", dimGrid.x, dimGrid.y);

  cudaChannelFormatDesc inputDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  inputTex.normalized = 0;
  cudaBindTexture(0, inputTex, input, inputDesc, num_input * sizeof(int));

  gen_index_kernel<<<dimGrid, dimBlock>>> (num_input, output);

  if (memtime) {
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
    cudaEventElapsedTime(&t, start, stop);
    printf("Time for gen index: %f\n", t);
    cudaEventDestroy(start);
    cudaEventDestroy(stop);
  }
}

#ifdef __cplusplus
}
#endif
