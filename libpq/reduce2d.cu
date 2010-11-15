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

texture<int, 1, cudaReadModeElementType> vecsTex;
__global__
void reduce2d_kernel(int num_vecs, int vec_len, int *output) {
  // We support _max_ 256 threads here.  May be wasteful, but I'm lazy about
  // learning to parameterize the kernel's shared memory usage for now...
  __shared__ int cache[256];

  // The algorithm:
  //
  // 1. Each thread is responsible for loading up one element of shared memory.
  //    It populates that element with the result of adding two elements from
  //    global memory.
  // 2. The thread block is 2D, with the x-dimension going within single vectors
  //    and the y-dim doing along the same element of multiple vectors.
  // 3. After we have that loaded up, we have half the threads sit idle while
  //    the others recursively add up their vectors in shared memory.
  // 4.

  int bx = blockDim.x;
  int by = blockDim.y;
  
  int idx = blockIdx.x * bx + threadIdx.x;
  int idy = blockIdx.y * by + threadIdx.y;

  if (idx < vec_len) {
    int firstvec = 2 * idy;
    int elidx = firstvec * vec_len + idx;
    int cacheid = bx * threadIdx.y + threadIdx.x;

    cache[cacheid] = 0;
    if (firstvec < num_vecs) {
      cache[cacheid] = tex1Dfetch(vecsTex, elidx);
    }
    if (firstvec + 1 < num_vecs) {
      cache[cacheid] += tex1Dfetch(vecsTex, elidx + vec_len);
    }
    __syncthreads();

    int cur_y = by >> 1;

    while (cur_y > 0) {
      if (threadIdx.y < cur_y) {
        cache[cacheid] += cache[cacheid + cur_y * bx];
      }
      __syncthreads();
      cur_y = cur_y >> 1;
    }

    // Output the result
    if (threadIdx.y == 0 && threadIdx.x < bx) {
      output[vec_len * blockIdx.y + idx] = cache[threadIdx.x];
    }
  }
}

void launch_reduce2d(int *vecs, int num_vecs, int vec_len, int *output,
                     int bw, int bh) {
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  // Alloc device copies
  int *curInput;
  int *curOutput;
  rslt = cudaMalloc((void**)&curInput, num_vecs * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Vecs: %d\n", rslt);
  rslt = cudaMemcpy(curInput, vecs, num_vecs * vec_len * sizeof(int),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy vecs to dev: %d\n", rslt);

  // Determine the shape of the block
  // TODO: Experiment here, find what's best
  //       One thing: every wasted thread horizontally is multiplied by
  //                  the number of vectors, which is a lot.  This is traded
  //                  off with the fact that wider blocks means better memory
  //                  coalescing.
  int pows_of_2[8] = {256, 128, 64, 32, 16, 8, 4, 2};
  int bwidth = bw;
  int bheight = bh;
  int mod, i;

  /*
  for (i = 0; i < 8; ++i) {
    mod = vec_len % pows_of_2[i];
    if (mod == 0 || pows_of_2[i] - mod < 3) {
      bwidth = pows_of_2[i];
      bheight = 256 / bwidth;
      break;
    }
  }
  */
  
  dim3 dimBlock(bwidth, bheight);
  dim3 dimGrid;

  int input_height = num_vecs;
  int output_height;
  cudaChannelFormatDesc inputDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  vecsTex.normalized = 0;

  cudaEventRecord(start, 0);
  
  while (input_height > 1) {
    output_height = safe_div(input_height, bheight * 2);
    dimGrid = dim3(safe_div(vec_len, bwidth), output_height);
    rslt = cudaMalloc((void**)&curOutput,
                      output_height * vec_len * sizeof(int));
    if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

    cudaBindTexture(0, vecsTex, curInput, inputDesc,
                    input_height * vec_len * sizeof(int));
    printf("launching kernel with grid, block dims: (%d,%d),(%d,%d)\n",
           dimGrid.x, dimGrid.y, dimBlock.x, dimBlock.y);
    reduce2d_kernel<<<dimGrid, dimBlock>>>
      (num_vecs, vec_len, curOutput);
    cudaUnbindTexture(vecsTex);
    
    cudaFree(curInput);
    curInput = curOutput;
    input_height = output_height;
  }

  cudaEventRecord(stop, 0);
  cudaEventSynchronize(stop);
  
  rslt = cudaMemcpy(output, curOutput, sizeof(int) * vec_len,
                    cudaMemcpyDeviceToHost);
  float t;
  cudaEventElapsedTime(&t, start, stop);
  printf("Time for kernels: %f\n", t);
  cudaEventDestroy(start);
  cudaEventDestroy(stop);

  cudaFree(curOutput);
}

#ifdef __cplusplus
}
#endif
