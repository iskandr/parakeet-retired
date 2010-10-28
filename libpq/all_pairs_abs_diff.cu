/*
 * Parallel Q Library
 *
 * (c) 2009-2010 Eric Hielscher
 *
 * Each-left each-right implementations.
 */

#include <cuda.h>
#include <stdio.h>

#define THREADS_PER_BLOCK 256
#define THREADS_PER_DIM 16

#ifdef __cplusplus
extern "C" {
#endif

int safe_div(int n, int d) {
  return (n + d - 1) / d;
}

__global__
void all_pairs_abs_diff_kernel(int *X, int left_len,
                               int *C, int right_len,
                               int vec_len, int *output) {
  int left_id  = blockIdx.x * THREADS_PER_DIM + threadIdx.x;
  int right_id = blockIdx.y * THREADS_PER_DIM + threadIdx.y;
  if (left_id < left_len && right_id < right_len) {
    int left_idx   = vec_len * left_id;
    int right_idx  = vec_len * right_id;
    int i;
    for (i = 0; i < vec_len; ++i) {
      output[left_id * (right_len * vec_len) + right_id * vec_len + i] =
        abs(X[left_idx + i] - C[right_idx + i]);
    }
  }
}

texture<int, 2, cudaReadModeElementType> leftTex;
texture<int, 2, cudaReadModeElementType> rightTex;
__global__
void all_pairs_abs_diff_tex_kernel(int left_len, int right_len,
                                   int vec_len, int *output) {
  int left_id = blockIdx.x * THREADS_PER_DIM + threadIdx.x;
  int right_id = blockIdx.y * THREADS_PER_DIM + threadIdx.y;
  if (left_id < left_len && right_id < right_len) {
    int i;
    for (i = 0; i < vec_len; ++i) {
      output[left_id * (right_len * vec_len) + right_id * vec_len + i] =
        abs(tex2D(leftTex, i, left_id) - tex2D(rightTex, i, right_id));
    }
  }
}

__global__
void all_pairs_abs_diff_tiling_1_kernel(int *left, int left_len,
                                        int *right, int right_len,
                                        int vec_len, int *output) {

  __shared__ int LeftS[THREADS_PER_DIM][THREADS_PER_DIM];
  __shared__ int RightS[THREADS_PER_DIM][THREADS_PER_DIM];

  int i, j;
  int left_id   = blockIdx.x * THREADS_PER_DIM + threadIdx.x;
  int right_id  = blockIdx.y * THREADS_PER_DIM + threadIdx.x;
  int left_start_idx  = left_id * vec_len + threadIdx.y;
  int right_start_idx = right_id * vec_len + threadIdx.y;
  int out_start_idx = (left_id * right_len +
                       blockIdx.y * THREADS_PER_DIM + threadIdx.y) * vec_len;

  for (i = 0; i < vec_len; i += THREADS_PER_DIM) {
    // Load tile
    if (threadIdx.y + i < vec_len) {
      if (left_id < left_len) {
        LeftS[threadIdx.x][threadIdx.y] = left[left_start_idx + i];
      }
      if (right_id < right_len) {
        RightS[threadIdx.x][threadIdx.y] = right[right_start_idx + i];
      }
    }
    __syncthreads();

    // Perform computation
    for (j = 0; j < THREADS_PER_DIM; ++j) {
      if (i + j < vec_len && left_id < left_len &&
          blockIdx.y * THREADS_PER_DIM + threadIdx.y < right_len) {
        output[out_start_idx + i + j] =
          abs(LeftS[threadIdx.x][j] - RightS[threadIdx.y][j]);
      }
    }
    __syncthreads();
  }
}

__global__
void all_pairs_abs_diff_tiling_kernel(int *left, int left_len,
                                      int *right, int right_len,
                                      int vec_len, int *output) {

  __shared__ int LeftS[THREADS_PER_DIM][THREADS_PER_DIM];
  __shared__ int RightS[THREADS_PER_DIM][THREADS_PER_DIM];

  int i, j;
  int left_id   = blockIdx.x * THREADS_PER_DIM + threadIdx.x;
  int right_id  = blockIdx.y * THREADS_PER_DIM + threadIdx.x;
  int left_start_idx  = left_id * vec_len + threadIdx.y;
  int right_start_idx = right_id * vec_len + threadIdx.y;
  
  int out_y = blockIdx.y * THREADS_PER_DIM;
  int out_idx = vec_len * (left_id * right_len + out_y) + threadIdx.y;

  for (i = 0; i < vec_len; i += THREADS_PER_DIM) {
    // Load tile
    if (threadIdx.y + i < vec_len) {
      if (left_id < left_len) {
        LeftS[threadIdx.x][threadIdx.y] = left[left_start_idx + i];
      }
      if (right_id < right_len) {
        RightS[threadIdx.x][threadIdx.y] = right[right_start_idx + i];
      }
    }
    __syncthreads();

    // Perform computation
    for (j = 0; j < THREADS_PER_DIM; ++j) {
      if (threadIdx.y + i < vec_len &&
          out_y + j < right_len && left_id < left_len) {
        output[out_idx + vec_len * j + i] =
          abs(LeftS[threadIdx.x][threadIdx.y] - RightS[j][threadIdx.y]);
      }
    }
    __syncthreads();
  }
}

void all_pairs_abs_diff(int *X, int left_len,
                        int *C, int right_len,
                        int vec_len, int *output) {
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  // Alloc device copies
  int *devX;
  int *devC;
  int *devOutput;
  rslt = cudaMalloc((void**)&devX, left_len * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev X: %d\n", rslt);
  rslt = cudaMalloc((void**)&devC, right_len * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev C: %d\n", rslt);
  // Copy data to device
  rslt = cudaMemcpy(devX, X, left_len * vec_len * sizeof(int),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy input: %d\n", rslt);
  rslt = cudaMemcpy(devC, C, right_len * vec_len * sizeof(int),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy input: %d\n", rslt);
  rslt = cudaMalloc((void**)&devOutput,
                    left_len * right_len * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Output: %d\n", rslt);

  //Invoke kernel
  dim3 dimBlock(THREADS_PER_DIM, THREADS_PER_DIM);
  int num_rows = left_len * right_len;
  int num_blocks = safe_div(num_rows, THREADS_PER_BLOCK);
  int gridX, gridY;
  gridX = safe_div(left_len, THREADS_PER_DIM);
  gridY = safe_div(right_len, THREADS_PER_DIM);
  dim3 dimGrid(gridX, gridY);
  printf("grid.x, grid.y: %d %d\n", dimGrid.x, dimGrid.y);
  cudaEventRecord(start, 0);

  
  all_pairs_abs_diff_kernel<<<dimGrid, dimBlock>>>
    (devX, left_len, devC, right_len, vec_len, devOutput);
  

  // Texture experiment
  /*
  cudaChannelFormatDesc leftDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  cudaChannelFormatDesc rightDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  leftTex.normalized = 0;
  rightTex.normalized = 0;
  cudaBindTexture2D(0, leftTex, devX, leftDesc,
                    vec_len, left_len, vec_len * sizeof(int));
  cudaBindTexture2D(0, rightTex, devC, rightDesc,
                    vec_len, right_len, vec_len * sizeof(float));
  all_pairs_abs_diff_tex_kernel<<<dimGrid, dimBlock>>>
    (left_len, right_len, vec_len, devOutput);
  */
  
  cudaEventRecord(stop, 0);
  cudaEventSynchronize(stop);

  rslt = cudaMemcpy(output, devOutput, sizeof(int) * num_rows * vec_len,
                    cudaMemcpyDeviceToHost);
  if (rslt != 0) printf("failed to copy output to host: %d\n", rslt);

  float t;
  cudaEventElapsedTime(&t, start, stop);
  printf("Time for kernel: %f\n", t);
  cudaEventDestroy(start);
  cudaEventDestroy(stop);

  cudaFree(devX);
  cudaFree(devC);
  cudaFree(devOutput);
}

#ifdef __cplusplus
}
#endif
