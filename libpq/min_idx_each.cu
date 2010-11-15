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

int safe_div(int n, int d) {
  return (n + d - 1) / d;
}

/**
  * Takes a (num_rows x num_cols) matrix D and computes the index of the first
  * occurance of the min in each row of D.
  */
__global__ void
min_idx_each_colmajor_kernel(int *D, int *output, int num_rows, int num_cols) {
  unsigned int tid = threadIdx.x;
  unsigned int linearThreadIdx =
    (blockIdx.x + (blockIdx.y * gridDim.x)) * THREADS_PER_BLOCK + tid;
  unsigned int start = linearThreadIdx;

  if (linearThreadIdx < num_rows) {
    int min = D[start];
    int idx_min = 0;
    int i, tmp, offset;
    offset = 0;
    for (i = 1; i < num_cols; ++i) {
      offset += num_rows;
      tmp = D[start+offset];
      if (min > tmp) {
        idx_min = i;
        min = tmp;
      }
    }

    output[linearThreadIdx] = idx_min;
  }
}

/**
  * Takes a (num_rows x num_cols) matrix D and computes the index of the first
  * occurance of the min in each row of D.
  */
__global__ void
min_idx_each_rowmajor_kernel(int *D, int *output, int num_rows, int num_cols) {
  unsigned int tid = threadIdx.x;
  unsigned int linearThreadIdx =
    (blockIdx.x + (blockIdx.y * gridDim.x)) * THREADS_PER_BLOCK + tid;
  unsigned int start = linearThreadIdx * num_cols;
  
  int i, tmp;
  if (linearThreadIdx < num_rows) {
    int min = D[start];
    int idx_min = 0;
    for (i = 1; i < num_cols; ++i) {
      tmp = D[start+i];
      if (min > tmp) {
        idx_min = i;
        min = tmp;
      }
    }

    output[linearThreadIdx] = idx_min;
  }
}

#ifdef __cplusplus
extern "C" {
#endif

void min_idx_each_rowmajor(int *D, int* output,
                           int num_rows, int num_cols) {
  // Call min-all
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  // Alloc device copies
  int *devInput;
  int *devOutput;
  rslt = cudaMalloc((void**)&devInput, num_rows * num_cols * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Input: %d\n", rslt);
  // Copy data to device
  rslt = cudaMemcpy(devInput, D, num_rows * num_cols * sizeof(int),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy input: %d\n", rslt);
  rslt = cudaMalloc((void**)&devOutput, num_rows * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Output: %d\n", rslt);

  //Invoke kernel
  dim3 dimBlock(THREADS_PER_BLOCK);
  int num_blocks = safe_div(num_rows, THREADS_PER_BLOCK);
  int gridX, gridY;
  if (num_blocks < 16384) {
    gridX = num_blocks;
    gridY = 1;
  } else {
    gridX = 16384;
    gridY = safe_div(num_blocks, 16384);
  }
  dim3 dimGrid(gridX, gridY);
  printf("grid.x, grid.y: %d %d\n", dimGrid.x, dimGrid.y);
  cudaEventRecord(start, 0);
  min_idx_each_colmajor_kernel<<<dimGrid, dimBlock>>>
    (devInput, devOutput, num_rows, num_cols);
  cudaEventRecord(stop, 0);
  
  rslt = cudaMemcpy(output, devOutput, sizeof(int) * num_rows,
                    cudaMemcpyDeviceToHost);
  if (rslt != 0) printf("failed to copy output to host: %d\n", rslt);

  cudaEventSynchronize(stop);

  float t;
  cudaEventElapsedTime(&t, start, stop);
  printf("Time for kernel: %f\n", t);
  cudaEventDestroy(start);
  cudaEventDestroy(stop);

  cudaFree(devInput);
  cudaFree(devOutput);
}

#ifdef __cplusplus
}
#endif
