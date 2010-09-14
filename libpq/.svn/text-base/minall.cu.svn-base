#ifdef __cplusplus
extern "C" {
#endif

#include "minall_kernel.cu"

void minall(float *input, int* output, int x_len, int y_len,
            int include_mem_in_time) {
  // Call min-all
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  // Alloc device copies
  float *devInput;
  int *devOutput;
  if (include_mem_in_time) {
    printf("Including memory in kernel time.\n");
    cudaEventRecord(start, 0);
  }
  rslt = cudaMalloc((void**)&devInput, x_len * y_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc dev Input: %d\n", rslt);
  rslt = cudaMalloc((void**)&devOutput, x_len * y_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Output: %d\n", rslt);

  // Copy data to device
  rslt = cudaMemcpy(devInput, input, x_len * y_len * sizeof(float),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy input: %d\n", rslt);

  // Invoke kernel
  dim3 dimBlock(256);
  dim3 dimGrid(y_len);
  printf("grid.x, grid.y: %d %d\n", dimGrid.x, dimGrid.y);
  if (!include_mem_in_time) {
    printf("Not including memory in kernel time.\n");
    cudaEventRecord(start, 0);
  }
  minall_kernel<<<dimGrid, dimBlock>>>
    (devInput, devOutput, x_len, y_len);
  if (!include_mem_in_time) {
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
  }

  rslt = cudaMemcpy(output, devOutput, x_len * y_len * sizeof(int),
                    cudaMemcpyDeviceToHost);
  if (rslt != 0) printf("failed to copy output to host: %d\n", rslt);

  if (include_mem_in_time) {
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
  }

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

