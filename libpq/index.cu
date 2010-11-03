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

//texture<int, 2, cudaReadModeElementType> vecsTex;
texture<int, 1, cudaReadModeElementType> vecsTex;
texture<int, 1, cudaReadModeElementType> idxsTex;
__global__
void index_kernel(int num_vecs, int vec_len, int num_idxs, int *output) {
  /* Our algorithm we'll try here: assign one thread per output element.
   * This should get perfect output coalescing, and the inputs are both in
   * textures so we should get fine input locality on the input reads.
   * TODO: look up how the texture caches actually work so that we are really
   *       getting efficiency in the vec reads.
   */
  int id = blockIdx.y * 16384 * THREADS_PER_BLOCK +
           blockIdx.x * THREADS_PER_BLOCK + threadIdx.x;
  if (id < num_idxs * vec_len) {
    int idx = id / vec_len;
    int offset = id - (idx * vec_len);
    int vec = tex1Dfetch(idxsTex, idx);
    //output[id] = tex2D(vecsTex, offset, vec);
    output[id] = tex1Dfetch(vecsTex, vec*vec_len + offset);
  }
}

void launch_index(int *vecs, int num_vecs, int *idxs, int num_idxs, int vec_len,
                  int *output) {
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  // Alloc device copies
  int *devVecs;
  int *devIdxs;
  int *devOutput;
  rslt = cudaMalloc((void**)&devVecs, num_vecs * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Vecs: %d\n", rslt);
  rslt = cudaMalloc((void**)&devIdxs, num_idxs * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Idxs: %d\n", rslt);
  // Copy data to device
  rslt = cudaMemcpy(devVecs, vecs, num_vecs * vec_len * sizeof(int),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy input: %d\n", rslt);
  rslt = cudaMemcpy(devIdxs, idxs, num_idxs * sizeof(int),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy input: %d\n", rslt);
  rslt = cudaMalloc((void**)&devOutput,
                    num_idxs * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Output: %d\n", rslt);

  //Invoke kernel
  dim3 dimBlock(THREADS_PER_BLOCK);
  int num_out_els = num_idxs * vec_len;
  int num_blocks = safe_div(num_out_els, THREADS_PER_BLOCK);
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

  cudaChannelFormatDesc vecsDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  cudaChannelFormatDesc idxsDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  vecsTex.normalized = 0;
  idxsTex.normalized = 0;
/*  cudaBindTexture2D(0, vecsTex, devVecs, vecsDesc,
                    vec_len, num_vecs, vec_len * sizeof(int));*/
  cudaBindTexture(0, vecsTex, devVecs, vecsDesc,
                  num_vecs * vec_len * sizeof(int));
  cudaBindTexture(0, idxsTex, devIdxs, idxsDesc, num_idxs * sizeof(int));
  printf("Num vecs, idxs, len_vec: %d %d %d\n",
         num_vecs, num_idxs, vec_len);
  cudaEventRecord(start, 0);
  index_kernel<<<dimGrid, dimBlock>>>
    (num_vecs, vec_len, num_idxs, devOutput);

  cudaEventRecord(stop, 0);
  cudaEventSynchronize(stop);

  rslt = cudaMemcpy(output, devOutput, sizeof(int) * num_idxs * vec_len,
                    cudaMemcpyDeviceToHost);
  if (rslt != 0) printf("failed to copy output to host: %d\n", rslt);

  float t;
  cudaEventElapsedTime(&t, start, stop);
  printf("Time for kernel: %f\n", t);
  cudaEventDestroy(start);
  cudaEventDestroy(stop);

  cudaFree(devVecs);
  cudaFree(devIdxs);
  cudaFree(devOutput);
}

void launch_index_dev(int *vecs, int num_vecs, int *idxs, int num_idxs,
                      int vec_len, int *output, int memtime) {
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
  int num_out_els = num_idxs * vec_len;
  int num_blocks = safe_div(num_out_els, THREADS_PER_BLOCK);
  int gridX, gridY;
  if (safe_div(num_out_els, THREADS_PER_BLOCK) > 16384) {
    gridX = 16384;
    gridY = safe_div(num_blocks, 16384);
  } else {
    gridX = num_blocks;
    gridY = 1;
  }
  dim3 dimGrid(gridX, gridY);
  printf("[index] grid.x, grid.y: %d %d\n", dimGrid.x, dimGrid.y);

  cudaChannelFormatDesc vecsDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  cudaChannelFormatDesc idxsDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  vecsTex.normalized = 0;
  idxsTex.normalized = 0;
/*  cudaBindTexture2D(0, vecsTex, vecs, vecsDesc,
                    vec_len, num_vecs, vec_len * sizeof(int));*/
  cudaBindTexture(0, vecsTex, vecs, vecsDesc,
                  num_vecs * vec_len * sizeof(int));
  cudaBindTexture(0, idxsTex, idxs, idxsDesc, num_idxs * sizeof(int));

  index_kernel<<<dimGrid, dimBlock>>> (num_vecs, vec_len, num_idxs, output);

  if (memtime) {
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
    cudaEventElapsedTime(&t, start, stop);
    printf("Time for index: %f\n", t);
    cudaEventDestroy(start);
    cudaEventDestroy(stop);
  }
}

#ifdef __cplusplus
}
#endif
