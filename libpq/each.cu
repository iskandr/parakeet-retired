/*
 * Parallel Q Library
 *
 * (c) 2009-2010 Eric Hielscher
 *
 * Each-left each-right implementations.
 */

#include <cuda.h>
#include <stdio.h>

#define THID() (blockDim.x * gridDim.x * blockIdx.y + \
                blockIdx.x * blockDim.x + \
                threadIdx.x)

#define THREADS_PER_DIM 16
#define MAX_THREADS_PER_DIM 16

#ifdef __cplusplus
extern "C" {
#endif

/*
__global__ void all_dists_naive_kernel(float *left, int left_len,
                                       float *right, int right_len,
                                       int vec_len, float *output) {
  int thid = THID();

  if (thid < left_len * right_len) {
    int left_idx   = vec_len * (thid / right_len);
    int right_idx  = vec_len * (thid - (right_len * (left_idx / vec_len)));
    float result = 0.0f;
    float intermediate = 0.0f;
    int i;
    for (i = 0; i < vec_len; ++i) {
      intermediate =
        left[left_idx + i] - right[right_idx + i];
      result += intermediate * intermediate;
    }
    output[thid] = sqrtf(result);
  }
}
*/

__global__ void all_dists_naive_kernel(float *left, int left_len,
                                       float *right, int right_len,
                                       int vec_len, float *output) {
  int left_id  = blockIdx.y * THREADS_PER_DIM + threadIdx.y;
  int right_id = blockIdx.x * THREADS_PER_DIM + threadIdx.x;
  if (left_id < left_len && right_id < right_len) {
    int left_idx   = vec_len * left_id;
    int right_idx  = vec_len * right_id;
    float result = 0.0f;
    float intermediate = 0.0f;
    int i;
    for (i = 0; i < vec_len; ++i) {
      intermediate =
        left[left_idx + i] - right[right_idx + i];
      result += intermediate * intermediate;
    }
    output[left_id * right_len + right_id] = sqrtf(result);
  }
}

void all_dists_naive(float *left,  int left_len,
                     float *right, int right_len,
                     int vec_len, float *output,
                     int include_mem_in_time) {
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  // Alloc device copies
  float *devLeft, *devRight, *devOutput;
  if (include_mem_in_time) {
    printf("Including memory in kernel time.\n");
    cudaEventRecord(start, 0);
  }
  rslt = cudaMalloc((void**)&devLeft, left_len * vec_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc dev left: %d\n", rslt);
  rslt = cudaMalloc((void**)&devRight, right_len * vec_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc dev right: %d\n", rslt);
  rslt = cudaMalloc((void**)&devOutput, left_len * right_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc dev output: %d\n", rslt);

  // Copy data to device
  rslt = cudaMemcpy(devLeft, left, left_len * vec_len * sizeof(float),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy left: %d\n", rslt);
  rslt = cudaMemcpy(devRight, right, right_len * vec_len * sizeof(float),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy output: %d\n", rslt);

  // Invoke kernel
  dim3 dimBlock(THREADS_PER_DIM, THREADS_PER_DIM);
  dim3 dimGrid((right_len + THREADS_PER_DIM - 1) / THREADS_PER_DIM,
               (left_len + THREADS_PER_DIM - 1) / THREADS_PER_DIM);

  if (!include_mem_in_time) {
    printf("Not including memory in kernel time.\n");
    cudaEventRecord(start, 0);
  }
  all_dists_naive_kernel<<<dimGrid, dimBlock>>>
    (devLeft, left_len, devRight, right_len, vec_len, devOutput);
  if (!include_mem_in_time) {
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
  }

  rslt = cudaMemcpy(output, devOutput, left_len * right_len * sizeof(float),
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

__global__ void all_dists_1_kernel(float *left, int left_len,
                                   float *right, int right_len,
                                   int vec_len, float *output) {

    __shared__ float LeftS[THREADS_PER_DIM][THREADS_PER_DIM];
    __shared__ float RightS[THREADS_PER_DIM][THREADS_PER_DIM];

    // The algorithm:
    //
    // - Every thread is still responsible for a single output element.
    // - However, we stride across the left and right vectors in 16x16 tiles.
    // - Thus every thread accumulates a local value for its output, which
    //   it writes out at the end.
    // - The outer for loop is for the vector length.
    // - The inner for loop is for accumulating the data from the given tile.
    int i, j;
    float result = 0.0f;
    float intermediate = 0.0f;
    int left_start_idx  =
      (blockIdx.y * THREADS_PER_DIM + threadIdx.y) * vec_len + threadIdx.x;
    int right_start_idx =
      (blockIdx.x * THREADS_PER_DIM + threadIdx.y) * vec_len + threadIdx.x;
    for (i = 0; i < vec_len; i += THREADS_PER_DIM) {
      // Load tile
      LeftS[threadIdx.x][threadIdx.y] = left[left_start_idx + i];
      RightS[threadIdx.x][threadIdx.y] = right[right_start_idx + i];
      __syncthreads();

      // Perform computation
      for (j = 0; j < THREADS_PER_DIM; ++j) {
        intermediate = LeftS[j][threadIdx.y] - RightS[j][threadIdx.x];
        result += intermediate * intermediate;
      }
      __syncthreads();
    }

    // Write output back to global memory
    output[(blockIdx.y * THREADS_PER_DIM + threadIdx.y)
           * gridDim.x * THREADS_PER_DIM +
           blockIdx.x * THREADS_PER_DIM + threadIdx.x] = sqrtf(result);
    
}

texture<float, 2, cudaReadModeElementType> leftTex;
texture<float, 2, cudaReadModeElementType> rightTex;
__global__ void all_dists_tex_kernel(int left_len, int right_len,
                                     int vec_len, float *output) {
  int left_id  = blockIdx.y * THREADS_PER_DIM + threadIdx.y;
  int right_id = blockIdx.x * THREADS_PER_DIM + threadIdx.x;
  if (left_id < left_len && right_id < right_len) {
    float result = 0.0f;
    float intermediate;
    int i;
    for (i = 0; i < vec_len; ++i) {
      intermediate = tex2D(leftTex, i, left_id) - tex2D(rightTex, i, right_id);
      result += intermediate * intermediate;
    }
    output[left_id * right_len + right_id] = sqrtf(result);
  }
}

__global__ void all_dists_final_kernel(float *left, int left_len,
                                       float *right, int right_len,
                                       int vec_len, float *output) {

    __shared__ float LeftS[THREADS_PER_DIM][THREADS_PER_DIM];
    __shared__ float RightS[THREADS_PER_DIM][THREADS_PER_DIM];

    // The algorithm:
    //
    // - Every thread is still responsible for a single output element.
    // - However, we stride across the left and right vectors in 16x16 tiles.
    // - Thus every thread accumulates a local value for its output, which
    //   it writes out at the end.
    // - The outer for loop is for the vector length.
    // - The inner for loop is for accumulating the data from the given tile.
    int i, j;
    float result = 0.0f;
    float intermediate = 0.0f;
    int left_start_idx  =
      (blockIdx.y * THREADS_PER_DIM + threadIdx.y) * vec_len + threadIdx.x;
    int right_start_idx =
      (blockIdx.x * THREADS_PER_DIM + threadIdx.y) * vec_len + threadIdx.x;
    for (i = 0; i + THREADS_PER_DIM <= vec_len; i += THREADS_PER_DIM) {
      // Load tile
      LeftS[threadIdx.x][threadIdx.y] = left[left_start_idx + i];
      RightS[threadIdx.x][threadIdx.y] = right[right_start_idx + i];
      __syncthreads();

      // Perform computation
      for (j = 0; j < THREADS_PER_DIM; ++j) {
        intermediate = LeftS[j][threadIdx.y] - RightS[j][threadIdx.x];
        result += intermediate * intermediate;
      }
      __syncthreads();
    }
    int offset = (vec_len / THREADS_PER_DIM) * THREADS_PER_DIM;
    if (offset != vec_len) {
      if (threadIdx.x + offset < vec_len) {
        LeftS[threadIdx.x][threadIdx.y] = left[left_start_idx + offset];
        RightS[threadIdx.x][threadIdx.y] = right[right_start_idx + offset];
      }
      __syncthreads();

      for (j = 0; j < (vec_len - offset); ++j) {
        intermediate = LeftS[j][threadIdx.y] - RightS[j][threadIdx.x];
        result += intermediate * intermediate;
      }
      __syncthreads();
    }

    // Write output back to global memory
    output[(blockIdx.y * THREADS_PER_DIM + threadIdx.y)
           * gridDim.x * THREADS_PER_DIM +
           blockIdx.x * THREADS_PER_DIM + threadIdx.x] = sqrtf(result);

}

__global__ void all_dists_fixed_vec_len_kernel(float *left, int left_len,
                                               float *right, int right_len,
                                               int vec_len, float *output) {

    __shared__ float LeftS[THREADS_PER_DIM][THREADS_PER_DIM];
    __shared__ float RightS[THREADS_PER_DIM][THREADS_PER_DIM];

    // The algorithm:
    //
    // - Every thread is still responsible for a single output element.
    // - However, we stride across the left and right vectors in 16x16 tiles.
    // - Thus every thread accumulates a local value for its output, which
    //   it writes out at the end.
    // - The outer for loop is for the vector length.
    // - The inner for loop is for accumulating the data from the given tile.
    int i, j;
    float result = 0.0f;
    float intermediate = 0.0f;
    int left_start_idx  =
      (blockIdx.y * THREADS_PER_DIM + threadIdx.y) * vec_len + threadIdx.x;
    int right_start_idx =
      (blockIdx.x * THREADS_PER_DIM + threadIdx.y) * vec_len + threadIdx.x;
    for (i = 0; i < vec_len; i += THREADS_PER_DIM) {
      // Load tile
      LeftS[threadIdx.x][threadIdx.y] = left[left_start_idx + i];
      RightS[threadIdx.x][threadIdx.y] = right[right_start_idx + i];
      __syncthreads();

      // Perform computation
      for (j = 0; j < THREADS_PER_DIM; ++j) {
        intermediate = LeftS[j][threadIdx.y] - RightS[j][threadIdx.x];
        result += intermediate * intermediate;
      }
      __syncthreads();
    }

    // Write output back to global memory
    output[(blockIdx.y * THREADS_PER_DIM + threadIdx.y)
           * gridDim.x * THREADS_PER_DIM +
           blockIdx.x * THREADS_PER_DIM + threadIdx.x] = sqrtf(result);
    
}

__global__ void all_dists_3_kernel(float *left, int left_len,
                                   float *right, int right_len,
                                   int vec_len, int tile_dim,
                                   float *output) {

    __shared__ float LeftS[MAX_THREADS_PER_DIM][MAX_THREADS_PER_DIM];
    __shared__ float RightS[MAX_THREADS_PER_DIM][MAX_THREADS_PER_DIM];

    // The algorithm:
    //
    // - Every thread is still responsible for a single output element.
    // - However, we stride across the left and right vectors in 16x16 tiles.
    // - Thus every thread accumulates a local value for its output, which
    //   it writes out at the end.
    // - The outer for loop is for the vector length.
    // - The inner for loop is for accumulating the data from the given tile.
    int i, j;
    int dim = tile_dim;
    float result = 0.0f;
    float intermediate = 0.0f;
    int left_start_idx  =
      (blockIdx.y * dim + threadIdx.y) * vec_len + threadIdx.x;
    int right_start_idx =
      (blockIdx.x * dim + threadIdx.y) * vec_len + threadIdx.x;
    for (i = 0; i < vec_len; i += dim) {
      // Load tile
      LeftS[threadIdx.x][threadIdx.y] = left[left_start_idx + i];
      RightS[threadIdx.x][threadIdx.y] = right[right_start_idx + i];
      __syncthreads();

      // Perform computation
      for (j = 0; j < dim; ++j) {
        intermediate = LeftS[j][threadIdx.y] - RightS[j][threadIdx.x];
        result += intermediate * intermediate;
      }
      __syncthreads();
    }

    // Write output back to global memory
    output[(blockIdx.y * dim + threadIdx.y)
           * gridDim.x * dim +
           blockIdx.x * dim + threadIdx.x] = sqrtf(result);
    
}

__global__ void all_dists_generic_kernel(float *left, int left_len,
                                         float *right, int right_len,
                                         int vec_len,
                                         int tile_x, int tile_y,
                                         float *output) {

    __shared__ float LeftS[MAX_THREADS_PER_DIM][MAX_THREADS_PER_DIM];
    __shared__ float RightS[MAX_THREADS_PER_DIM][MAX_THREADS_PER_DIM];

    // The algorithm:
    //
    // - Every thread is still responsible for a single output element.
    // - However, we stride across the left and right vectors in 16x16 tiles.
    // - Thus every thread accumulates a local value for its output, which
    //   it writes out at the end.
    // - The outer for loop is for the vector length.
    // - The inner for loop is for accumulating the data from the given tile.
    int i, j;
    float result = 0.0f;
    float intermediate = 0.0f;
    int left_row = blockIdx.y * tile_y + threadIdx.y;
    int right_row = blockIdx.x * tile_y + threadIdx.y;
    int left_start_idx  =
      (blockIdx.y * tile_y + threadIdx.y) * vec_len + threadIdx.x;
    int right_start_idx =
      (blockIdx.x * tile_y + threadIdx.y) * vec_len + threadIdx.x;
    
    for (i = 0; i < vec_len; i += tile_x) {
      // Load tile
      if (threadIdx.x + i < vec_len && right_row < right_len && left_row < left_len) {
        LeftS[threadIdx.y][threadIdx.x] = left[left_start_idx + i];
        RightS[threadIdx.y][threadIdx.x] = right[right_start_idx + i];
      }
      __syncthreads();

      // Perform computation
      for (j = 0; j < tile_x && i + j < vec_len; ++j) {
        intermediate = LeftS[threadIdx.y][j] - RightS[threadIdx.x][j];
        result += intermediate * intermediate;
      }
      __syncthreads();
    }

    if (blockIdx.y * blockDim.y + threadIdx.y < left_len &&
        blockIdx.x * blockDim.x + threadIdx.x < right_len) {
      // Write output back to global memory
      output[(blockIdx.y * blockDim.y + threadIdx.y) * gridDim.x * blockDim.x +
             blockIdx.x * blockDim.x + threadIdx.x] = sqrtf(result);
    }
}

/**
 * Note: For now, this will only work with multiple-of-THREADS_PER_DIM? lens.
 */
void all_dists_1(float *left,  int left_len,
                 float *right, int right_len,
                 int vec_len, float *output,
                 int include_mem_in_time) {
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  // Alloc device copies
  float *devLeft, *devRight, *devOutput;
  if (include_mem_in_time) {
    printf("Including memory in kernel time.\n");
    cudaEventRecord(start, 0);
  }
  rslt = cudaMalloc((void**)&devLeft, left_len * vec_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc left: %d\n", rslt);
  rslt = cudaMalloc((void**)&devRight, right_len * vec_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc right: %d\n", rslt);
  rslt = cudaMalloc((void**)&devOutput, left_len * right_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

  // Copy data to device
  rslt = cudaMemcpy(devLeft, left, left_len * vec_len * sizeof(float),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy left: %d\n", rslt);
  rslt = cudaMemcpy(devRight, right, right_len * vec_len * sizeof(float),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy output: %d\n", rslt);

  // Invoke kernel
  int tile_x = 16;
  int tile_y = 16;
  if (vec_len < 10) tile_x = 8;
  dim3 dimBlock(tile_y, tile_y);
  dim3 dimGrid((right_len + tile_y - 1) / tile_y,
               (left_len + tile_y - 1) / tile_y);

  if (!include_mem_in_time) {
    printf("Not including memory in kernel time.\n");
    cudaEventRecord(start, 0);
  }

/*  all_dists_3_kernel<<<dimGrid, dimBlock>>>
    (devLeft, left_len, devRight, right_len, vec_len, 16, devOutput);*/
  /*all_dists_final_kernel<<<dimGrid, dimBlock>>>
    (devLeft, left_len, devRight, right_len, vec_len, devOutput);*/

  // Texture experiment
  
  cudaChannelFormatDesc leftDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindFloat);
  cudaChannelFormatDesc rightDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindFloat);
  leftTex.normalized = 0;
  rightTex.normalized = 0;
  cudaBindTexture2D(0, leftTex, devLeft, leftDesc,
                    vec_len, left_len, vec_len * sizeof(float));
  cudaBindTexture2D(0, rightTex, devRight, rightDesc,
                    vec_len, right_len, vec_len * sizeof(float));
  all_dists_tex_kernel<<<dimGrid, dimBlock>>>
    (left_len, right_len, vec_len, devOutput);
  

  if (!include_mem_in_time) {
    rslt = cudaThreadSynchronize();
    if (rslt != 0) printf("kernel launch failed: %d\n", rslt);
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
  }

  rslt = cudaMemcpy(output, devOutput, left_len * right_len * sizeof(float),
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

__global__ void all_dists_2_kernel(float *left, int left_len,
                                   float *right, int right_len,
                                   int vec_len, float *output) {

    __shared__ float LeftS[THREADS_PER_DIM*2][THREADS_PER_DIM*2];
    __shared__ float RightS[THREADS_PER_DIM*2][THREADS_PER_DIM*2];

    // The algorithm:
    //
    // - Every thread is still responsible for a single output element.
    // - However, we stride across the left and right vectors in 16x16 tiles.
    // - Thus every thread accumulates a local value for its output, which
    //   it writes out at the end.
    // - The outer for loop is for the vector length.
    // - The inner for loop is for accumulating the data from the given tile.
    int i, j;
    float result[4] = {0.0f, 0.0f, 0.0f, 0.0f};
    float intermediate;
    int left_start_idx  =
      (blockIdx.y * THREADS_PER_DIM*2 + threadIdx.y) * vec_len + threadIdx.x;
    int right_start_idx =
      (blockIdx.x * THREADS_PER_DIM*2 + threadIdx.y) * vec_len + threadIdx.x;
    for (i = 0; i < vec_len; i += THREADS_PER_DIM*2) {
      // Load tile
      LeftS[threadIdx.x][threadIdx.y] = left[left_start_idx + i];
      RightS[threadIdx.x][threadIdx.y] = right[right_start_idx + i];
      LeftS[threadIdx.x+THREADS_PER_DIM][threadIdx.y] =
        left[left_start_idx + THREADS_PER_DIM + i];
      RightS[threadIdx.x+THREADS_PER_DIM][threadIdx.y] =
        right[right_start_idx + THREADS_PER_DIM + i];
      LeftS[threadIdx.x][threadIdx.y+THREADS_PER_DIM] =
        left[left_start_idx + THREADS_PER_DIM * vec_len + i];
      RightS[threadIdx.x][threadIdx.y+THREADS_PER_DIM] =
        right[right_start_idx + THREADS_PER_DIM * vec_len + i];
      LeftS[threadIdx.x+THREADS_PER_DIM][threadIdx.y+THREADS_PER_DIM] =
        left[left_start_idx + THREADS_PER_DIM * vec_len + THREADS_PER_DIM + i];
      RightS[threadIdx.x+THREADS_PER_DIM][threadIdx.y+THREADS_PER_DIM] =
        right[right_start_idx + THREADS_PER_DIM * vec_len + THREADS_PER_DIM + i];
      __syncthreads();

      // Perform computation
      for (j = 0; j < THREADS_PER_DIM; ++j) {
        intermediate = LeftS[j][threadIdx.y] - RightS[j][threadIdx.x];
        result[0] += intermediate * intermediate;
      }
      __syncthreads();
    }

    // Write output back to global memory
    output[(blockIdx.y * THREADS_PER_DIM + threadIdx.y)
           * gridDim.x * THREADS_PER_DIM +
           blockIdx.x * THREADS_PER_DIM + threadIdx.x] = sqrtf(result[0]);
    
}

/**
 * Note: For now, this will only work with multiple-of-THREADS_PER_DIM? lens.
 */
void all_dists_2(float *left,  int left_len,
                 float *right, int right_len,
                 int vec_len, float *output,
                 int include_mem_in_time) {
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  // Alloc device copies
  float *devLeft, *devRight, *devOutput;
  if (include_mem_in_time) {
    printf("Including memory in kernel time.\n");
    cudaEventRecord(start, 0);
  }
  rslt = cudaMalloc((void**)&devLeft, left_len * vec_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc left: %d\n", rslt);
  rslt = cudaMalloc((void**)&devRight, right_len * vec_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc right: %d\n", rslt);
  rslt = cudaMalloc((void**)&devOutput, left_len * right_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

  // Copy data to device
  rslt = cudaMemcpy(devLeft, left, left_len * vec_len * sizeof(float),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy left: %d\n", rslt);
  rslt = cudaMemcpy(devRight, right, right_len * vec_len * sizeof(float),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy output: %d\n", rslt);

  // Invoke kernel
  int num_threads = left_len * right_len;
  dim3 dimBlock(THREADS_PER_DIM, THREADS_PER_DIM);
  dim3 dimGrid((right_len + THREADS_PER_DIM - 1) / THREADS_PER_DIM,
               (left_len + THREADS_PER_DIM - 1) / THREADS_PER_DIM);
  printf("grid.x, grid.y: %d %d\n", dimGrid.x, dimGrid.y);

  if (!include_mem_in_time) {
    printf("Not including memory in kernel time.\n");
    cudaEventRecord(start, 0);
  }

  all_dists_1_kernel<<<dimGrid, dimBlock>>>
    (devLeft, left_len, devRight, right_len, vec_len, devOutput);

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

