/*
 * Parallel Q Library
 *
 * (c) 2009-2010 Eric Hielscher
 *
 * Hand-made reference implementation of K-Means clustering.
 *
 * Notes:
 *
 * - Create the following global storage:
 * -- Input X array
 * -- Centroid vector array
 * -- Centroid old vector array
 *
 * - Algorithm:
 *
 * 1. Calculate all distances between input vectors and centroids
 *    (using tiled kernel)
 * 2. Calculate for each vector the centroid to which it is nearest
 * 3. Calculate the averages of each of the new groups to find new centroids
 * 4. Calculate the change
 *
 * We create 
 */

#include <cuda.h>
#include <stdio.h>

#define THID() (blockDim.x * gridDim.x * blockIdx.y + \
                blockIdx.x * blockDim.x + \
                threadIdx.x)

#ifdef __cplusplus
extern "C" {
#endif

/**
 * This only works at the moment for k = 512 and vec_len = 32.
 *
 * Assumes that we have
 *
 * - x_len * vec_len * sizeof(float) storage in X
 * - k * vec_len * sizeof(float) storage in clusters
 * - x_len * sizeof(int) storage in indices
 * - k * sizeof(int) storage in idx_starts
 *
 * Creates a bunch of intermediates as necessary.
 */
void kmeans(float *X, int x_len, int vec_len,
            float *clusters, int *indices, int *idx_starts,
            int k, float tol, int max_iters) {
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  // Generate initial clusters
  int i, j, idx, repeat;
  for (i = 0; i < k; ++i) {
    repeat = 0;
    do {
      idx = rand() % x_len;
      for (j = 0; j < i; ++j) {
        if (idx_starts[j] == idx) {
          repeat = 1;
          break;
        }
      }
    } while (repeat == 1);
    idx_starts[i] = idx;
    for (j = 0; j < vec_len; ++j) {
      clusters[vec_len * i + j] = X[vec_len * idx + j];
    }
  }

  // Alloc device memory
  float *devX, *devM, *devOldM, *devDiffs, *devValids, *devInterm, *devChange;
  if (include_mem_in_time) {
    printf("Including memory in kernel time.\n");
    cudaEventRecord(start, 0);
  }
  rslt = cudaMalloc((void**)&devX, x_len * vec_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc left: %d\n", rslt);
  rslt = cudaMalloc((void**)&devM, k * vec_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc right: %d\n", rslt);
  rslt = cudaMalloc((void**)&devOldM, k * vec_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);
  rslt = cudaMalloc((void**)&devValids, k * x_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);
  rslt = cudaMalloc((void**)&devChange, sizeof(float));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

  // Copy data to device
  rslt = cudaMemcpy(devX, X, x_len * vec_len * sizeof(float),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy left: %d\n", rslt);
  rslt = cudaMemcpy(devM, clusters, k * vec_len * sizeof(float),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy output: %d\n", rslt);
  
  // Timing
  if (!include_mem_in_time) {
    printf("Not including memory in kernel time.\n");
    cudaEventRecord(start, 0);
  }

  /*
   * Main Algorithm
   */
  float change = 10e30f;
  float *swapM;
  dim3 dimBlock, dimGrid;

  // Loop until either condition isn't met
  i = 0;
  while (change > tol && i < max_iters) {
    // First, find the nearest centroid for all vectors
    dimBlock = dim3(16, 16);
    dimGrid  = dim3((x_len + 15) / 16, (k + 15) / 16);
    printf("each diff grid.x, grid.y: %d %d\n", dimGrid.x, dimGrid.y);

    rslt = cudaMalloc((void**)&devDiffs, k * x_len * sizeof(int));
    if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

    all_dists_1_kernel<<<dimGrid, dimBlock>>>
      (devX, x_len, devM, k, vec_len, devDiffs);
      
    // Next, build the valids matrix from the diffs matrix
    dimBlock = dim3(256);
    dimGrid  = dim3(k);
    
    minall_kernel<<<dimGrid, dimBlock>>>
      (devDiffs, devValids, x_len, k);
    
    // At this point we can free the diffs matrix
    cudaFree(devDiffs);
    
    // Now perform the first multi-index-reduce using the valids matrix
    dimBlock = dim3(512);
    dimGrid  = dim3(k, x_len);
    
    multi_index_compact_kernel<<<dimGrid, dimBlock>>>
      (devX, devValids, k, 
  }

  // Cleanup
  if (!include_mem_in_time) {
    rslt = cudaThreadSynchronize();
    if (rslt != 0) printf("kernel launch failed: %d\n", rslt);
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
  }

  rslt = cudaMemcpy(output, devOutput, num_threads * sizeof(float),
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

  cudaFree(devLeft);
  cudaFree(devRight);
  cudaFree(devOutput);
}

#ifdef __cplusplus
}
#endif

