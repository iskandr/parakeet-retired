#ifdef __cplusplus
extern "C" {
#endif

#include "multi_index_kernel.cu"

void multi_index_reduce(float *input, float* output, int *valids,
                        int vec_len, int num_vecs, int k,
                        int include_mem_in_time) {
  
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  // Alloc device copies
  float *devInput;
  int   *devValids;
  float *devInterm1;
  float *devInterm2;
  if (include_mem_in_time) {
    printf("Including memory in kernel time.\n");
    cudaEventRecord(start, 0);
  }
  rslt = cudaMalloc((void**)&devInput, vec_len * num_vecs * sizeof(float));
  if (rslt != 0) printf("failed to malloc dev Input: %d\n", rslt);
  rslt = cudaMalloc((void**)&devValids, k * num_vecs * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Valids: %d\n", rslt);
  rslt = cudaMalloc((void**)&devInterm1,
    num_vecs * 2 * k * sizeof(float));
  if (rslt != 0) printf("failed to malloc dev Interm: %d\n", rslt);

  // Copy data to device
  rslt = cudaMemcpy(devInput, input, vec_len * num_vecs * sizeof(float),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy input: %d\n", rslt);
  rslt = cudaMemcpy(devValids, valids, num_vecs * k * sizeof(int),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy valids: %d\n", rslt);

  // Invoke kernel
  dim3 dimBlock(512);
  dim3 dimGrid(k, num_vecs / 16);
  printf("grid.x, grid.y: %d %d\n", dimGrid.x, dimGrid.y);
  if (!include_mem_in_time) {
    printf("Not including memory in kernel time.\n");
    cudaEventRecord(start, 0);
  }
  
  // Launch the compaction kernel
  multi_index_compact_kernel<<<dimGrid, dimBlock>>>
    (devInput, devValids, k, devInterm1);
  rslt = cudaThreadSynchronize();
  if (rslt != 0) printf("Failed to launch compact kernel: %d\n", rslt);
  
  // Test
/*  float *gold = (float*)malloc(num_vecs * 2 * k * sizeof(float));
  float *sums = (float*)malloc(num_vecs * 2 * k * sizeof(float));
  cudaMemcpy(sums, devInterm1, num_vecs * 2 * k * sizeof(float),
             cudaMemcpyDeviceToHost);
  int i, j, k;
  float diff;
  
  for (i = 0; i < num_cs; ++i) {
    for (j = 0; j < num_vecs; j += 16) {
      for (k = 0; k < 32; ++k) {
        gold[i * num_vecs / 16 + 
      for (k = 0; k < 16; ++k) {
        
  
  for (i = 0; i < num_cs; ++i) {
    for (j = 0; j < num_vecs / 16; ++j) {
      diff = 0.0f;
      for (k = 0; k < 32; ++k) {
  */      
  
  // Recursively launch the reduction kernels
  int interm_width = num_vecs / 16;
  dimBlock = dim3(256);
  while (interm_width > 1) {
    printf("Interm_width: %d\n", interm_width);
    dimGrid = dim3(interm_width / 16, k);
    interm_width /= 16;
    rslt = cudaMalloc((void**)&devInterm2,
      interm_width * 32 * k * sizeof(float));
    if (rslt != 0) printf("failed to alloc interim2: %d\n", rslt);
    multi_index_reduce_kernel<<<dimGrid, dimBlock>>>
      (devInterm1, devInterm2, k);
    rslt = cudaThreadSynchronize();
    printf("Launched reduce with params grid, block: %d %d, %d\n",
           dimGrid.x, dimGrid.y, dimBlock.x);
    if (rslt != 0) printf("Failed to launch reduce kernel: %d\n", rslt);
    rslt = cudaFree(devInterm1);
    if (rslt != 0) printf("Failed to dealloc interm2: %d\n", rslt);
    devInterm1 = devInterm2;
  }
    
  if (!include_mem_in_time) {
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
  }

  rslt = cudaMemcpy(output, devInterm1, vec_len * k * sizeof(int),
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
  cudaFree(devInterm1);
}

#ifdef __cplusplus
}
#endif

