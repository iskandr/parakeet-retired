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

texture<float, 1, cudaReadModeElementType> inputTex;
__global__
void where_kernel(int test_val, int input_len, int *output) {
  int id = blockIdx.y * 16384 * THREADS_PER_BLOCK +
           blockIdx.x * THREADS_PER_BLOCK + threadIdx.x;
  if (id < input_len) {
    int in = tex1Dfetch(inputTex, id);
    if (in == test_val) {
      output[id] = 1;
    } else {
      output[id] = 0;
    }
  }
}

void launch_where(int test_val, int *input, int input_len, int *output) {
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  // Alloc device copies
  int *devInput;
  int *devOutput;
  rslt = cudaMalloc((void**)&devInput, input_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Input: %d\n", rslt);
  // Copy data to device
  rslt = cudaMemcpy(devInput, input, input_len * sizeof(int),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy input: %d\n", rslt);
  rslt = cudaMalloc((void**)&devOutput, input_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Output: %d\n", rslt);

  //Invoke kernel
  dim3 dimBlock(THREADS_PER_BLOCK);
  int num_blocks = safe_div(input_len, THREADS_PER_BLOCK);
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

  cudaChannelFormatDesc inputDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  inputTex.normalized = 0;
  cudaBindTexture(0, inputTex, devInput, inputDesc, input_len * sizeof(int));
  cudaEventRecord(start, 0);
  where_kernel<<<dimGrid, dimBlock>>> (test_val, input_len, devOutput);

  cudaEventRecord(stop, 0);
  cudaEventSynchronize(stop);

  rslt = cudaMemcpy(output, devOutput, sizeof(int) * input_len,
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

void launch_where_dev(int test_val, int *input, int input_len, int *output,
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
  int num_blocks = safe_div(input_len, THREADS_PER_BLOCK);
  int gridX, gridY;
  if (num_blocks > 16384) {
    gridX = 16384;
    gridY = safe_div(num_blocks, 16384);
  } else {
    gridX = num_blocks;
    gridY = 1;
  }
  dim3 dimGrid(gridX, gridY);
  printf("[where] grid.x, grid.y: %d %d\n", dimGrid.x, dimGrid.y);

  cudaChannelFormatDesc inputDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  inputTex.normalized = 0;
  cudaBindTexture(0, inputTex, input, inputDesc, input_len * sizeof(int));
  
  where_kernel<<<dimGrid, dimBlock>>> (test_val, input_len, output);

  if (memtime) {
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
    cudaEventElapsedTime(&t, start, stop);
    printf("Time for where: %f\n", t);
    cudaEventDestroy(start);
    cudaEventDestroy(stop);
  }
}

#ifdef __cplusplus
}
#endif
