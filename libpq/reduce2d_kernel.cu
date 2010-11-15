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

texture<float, 1, cudaReadModeElementType> reduceVecsTex;
__global__
void reduce2d_kernel(int num_vecs, int vec_len, float *output) {
  // We support _max_ 256 threads here.  May be wasteful, but I'm lazy about
  // learning to parameterize the kernel's shared memory usage for now...
  __shared__ float cache[256];

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

    if (firstvec < num_vecs) {
      cache[cacheid] = tex1Dfetch(reduceVecsTex, elidx);
    }
    if (firstvec + 1 < num_vecs) {
      cache[cacheid] += tex1Dfetch(reduceVecsTex, elidx + vec_len);
    }
    __syncthreads();

    int cur_y = by >> 1;
    while (cur_y > 0) {
      if (threadIdx.y < cur_y && (idy + cur_y) * 2 < num_vecs) {
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

void launch_reduce2d(float *vecs, int num_vecs, int vec_len, float *output) {
  cudaError_t rslt;
  
  // Determine the shape of the block
  // TODO: Experiment here, find what's best
  //       One thing: every wasted thread horizontally is multiplied by
  //                  the number of vectors, which is a lot.  This is traded
  //                  off with the fact that wider blocks means better memory
  //                  coalescing.
  int pows_of_2[8] = {256, 128, 64, 32, 16, 8, 4, 2};
  int bwidth;
  int bheight;
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
  bwidth = 16;
  bheight = 16;

  dim3 dimBlock(bwidth, bheight);
  dim3 dimGrid;

  int input_height = num_vecs;
  int output_height = safe_div(input_height, bheight * 2);
  cudaChannelFormatDesc inputDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindFloat);
  reduceVecsTex.normalized = 0;

  float *curInput = vecs;
  float *curOutput;
  int notfirst = 0;

  while (output_height > 1) {
    dimGrid = dim3(safe_div(vec_len, bwidth), output_height);
    rslt = cudaMalloc((void**)&curOutput,
                      output_height * vec_len * sizeof(float));
    check_err(rslt, "failed to malloc reduce output");

    cudaBindTexture(0, reduceVecsTex, curInput, inputDesc,
                    input_height * vec_len * sizeof(float));
    reduce2d_kernel<<<dimGrid, dimBlock>>> (input_height, vec_len, curOutput);
    cudaUnbindTexture(reduceVecsTex);

    if (notfirst) {
      cudaFree(curInput);
    } else {
      notfirst = 1;
    }
    curInput = curOutput;
    input_height = output_height;
    output_height = safe_div(input_height, bheight * 2);
  }

  // We do the final iteration here so that we can fill the passed-in output
  dimGrid = dim3(safe_div(vec_len, bwidth), output_height);
  cudaBindTexture(0, reduceVecsTex, curInput, inputDesc,
                  input_height * vec_len * sizeof(float));
  reduce2d_kernel<<<dimGrid, dimBlock>>> (input_height, vec_len, output);
  cudaUnbindTexture(reduceVecsTex);
  if (notfirst) cudaFree(curInput);
}

#ifdef __cplusplus
}
#endif
