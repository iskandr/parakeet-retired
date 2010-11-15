#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "all_pairs_dists_1dtex_kernel.cu"
#include "all_pairs_dists_kernel.cu"
#include "base.h"

void usage(void) {
  printf("Usage:\n"
         "--------"
         "-x n       : set number of vectors\n"
         "-y n       : set number of centroids\n"
         "-l n       : set vector length\n"
         "-d         : do 1D\n"
         "-m         : include memory transfer in timing\n"
         );
}

int main(int argc, char **argv) {
  int x_len = 10000;
  int c_len = 128;
  int vec_len = 128;
  int include_mem = 0;
  int doid = 0;
  int i;

  // Process command line
  for (i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "-x")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      x_len = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-y")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      c_len = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-l")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      vec_len = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-d")) {
      doid = 1;
    } else if (!strcmp(argv[i], "-m")) {
      include_mem = 1;
    }
  }

  int *X;
  int *C;
  float *output;
  cudaError_t rslt;
  printf("Allocating host mem\n");
  rslt = cudaMallocHost((void**)&X, x_len * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc X: %d\n", rslt);
  rslt = cudaMallocHost((void**)&C, c_len * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc C: %d\n", rslt);
  rslt = cudaMallocHost((void**)&output, x_len * c_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

  // Initialize array
  printf("initing host input\n");
  for (i = 0; i < x_len * vec_len; ++i) {
    X[i] = rand() % 10;
  }
  for (i = 0; i < c_len * vec_len; ++i) {
    C[i] = rand() % 10;
  }

  float t;
  struct timeval *pqst, *pqe;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  int *devX;
  int *devC;
  float *devOut;
  int same = 1;
  int j, k;
  float intermediate, result;
  float tol = 0.01f;

  if (doid) {

    printf("Processing %d vectors, %d centroids, with length %d\n",
          x_len, c_len, vec_len);
    printf("Running 1D texture version\n");

    if (include_mem) {
      printf("Including Memory in xFer & allocation in time\n");
      cudaEventRecord(start, 0);
      pqst = pq_gettime();
    }

    rslt = cudaMalloc((void**)&devX, x_len * vec_len * sizeof(int));
    check_err(rslt, "Unable to malloc devX");
    rslt = cudaMalloc((void**)&devC, c_len * vec_len * sizeof(int));
    check_err(rslt, "Unable to malloc devC");
    rslt = cudaMalloc((void**)&devOut, x_len * c_len * sizeof(float));
    check_err(rslt, "Unable to malloc devOut");
    rslt = cudaMemcpy(devX, X, x_len * vec_len * sizeof(int),
                      cudaMemcpyHostToDevice);
    check_err(rslt, "Unable to copy X to device");
    rslt = cudaMemcpy(devC, C, c_len * vec_len * sizeof(int),
                      cudaMemcpyHostToDevice);
    check_err(rslt, "Unable to copy C to device");

    if (!include_mem) {
      cudaEventRecord(start, 0);
      pqst = pq_gettime();
    }

    cudaChannelFormatDesc OneDXDesc =
      cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
    cudaChannelFormatDesc OneDCDesc =
      cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
    rslt = cudaBindTexture(0, allDistsLeft1DTex, devX, OneDXDesc,
                          x_len * vec_len * sizeof(int));
    check_err(rslt, "Unable to bind 1D X texture");
    rslt = cudaBindTexture(0, allDistsRight1DTex, devC, OneDCDesc,
                          c_len * vec_len * sizeof(int));
    check_err(rslt, "Unable to bind 1D C texture");

    dim3 dim1DBlock(THREADS_PER_DIM, THREADS_PER_DIM);
    dim3 dim1DGrid(safe_div(c_len, THREADS_PER_DIM),
                  safe_div(x_len, THREADS_PER_DIM));

    all_pairs_dists_1dtex_kernel<<<dim1DGrid, dim1DBlock>>>
      (x_len, c_len, vec_len, devOut);

    if (!include_mem) {
      cudaEventRecord(stop, 0);
      cudaEventSynchronize(stop);
      pqe = pq_gettime();
    }

    rslt = cudaMemcpy(output, devOut, x_len * c_len * sizeof(float),
                      cudaMemcpyDeviceToHost);
    check_err(rslt, "Error copying 1D output to host");

    if (include_mem) {
      cudaEventRecord(stop, 0);
      cudaEventSynchronize(stop);
      pqe = pq_gettime();
    }

    cudaUnbindTexture(allDistsLeft1DTex);
    cudaUnbindTexture(allDistsRight1DTex);

    cudaEventElapsedTime(&t, start, stop);
    printf("Wall time for 1D texture: %fms\n", 1000*pq_diff_timers(pqst, pqe));
    printf("GPU time for 1D texture: %fms\n", t);

    // Check validity
    printf("checking validity for 1D case\n");
    for (i = 0; i < x_len; ++i) {
      for (j = 0; j < c_len; ++j) {
        result = 0.0f;
        for (k = 0; k < vec_len; ++k) {
          intermediate = X[i*vec_len + k] - C[j*vec_len + k];
          result += intermediate * intermediate;
        }
        result = sqrt(result);
        if (same && ((fabs(result - output[i*c_len + j]) / result) > tol)) {
          same = 0;
          printf("Different output at X %d, C %d: %f, %f\n", i, j, result,
                output[i*c_len + j]);
        }
      }
    }

    if (same) printf("1D Same!\n");
  
    cudaFree(devX);
    cudaFree(devC);
    cudaFree(devOut);
  }

  if (include_mem) {
    cudaEventRecord(start, 0);
    pqst = pq_gettime();
  }

  
  size_t x_pitch, c_pitch;
  rslt = cudaMallocPitch((void**)&devX, &x_pitch, vec_len * sizeof(int),
                         x_len);
  check_err(rslt, "Unable to malloc devX");
  rslt = cudaMallocPitch((void**)&devC, &c_pitch, vec_len * sizeof(int),
                         c_len);
  check_err(rslt, "Unable to malloc devC");

  printf("X and C pitches: %d, %d\n", x_pitch, c_pitch);

  cudaChannelFormatDesc TwoDXDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  cudaChannelFormatDesc TwoDCDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);

  /*
  cudaArray *AX, *AC;

  rslt = cudaMallocArray(&AX, &TwoDXDesc, vec_len, x_len);
  check_err(rslt, "Unable to malloc X Array");
  rslt = cudaMallocArray(&AC, &TwoDCDesc, vec_len, c_len);
  check_err(rslt, "Unable to malloc C Array");
                         
  rslt = cudaMalloc((void**)&devOut, x_len * c_len * sizeof(float));
  check_err(rslt, "Unable to malloc devOut");

  */
  
  rslt = cudaMemcpy2D(devX, x_pitch, X, vec_len * sizeof(int),
                      vec_len * sizeof(int), x_len, cudaMemcpyHostToDevice);
  check_err(rslt, "Unable to copy X to device");
  rslt = cudaMemcpy2D(devC, c_pitch, C, vec_len * sizeof(int),
                      vec_len * sizeof(int), c_len, cudaMemcpyHostToDevice);
  check_err(rslt, "Unable to copy C to device");
  

  /*
  rslt = cudaMemcpyToArray(AX, 0, 0, X, vec_len * x_len * sizeof(int),
                           cudaMemcpyHostToDevice);
  check_err(rslt, "Unable to copy to X Array");
  rslt = cudaMemcpyToArray(AC, 0, 0, C, vec_len * c_len * sizeof(int),
                           cudaMemcpyHostToDevice);
  check_err(rslt, "Unable to copy to C Array");
  */

  if (!include_mem) {
    cudaEventRecord(start, 0);
    pqst = pq_gettime();
  }
  
  rslt = cudaBindTexture2D(0, allDistsRight2DTex, devC, TwoDCDesc,
                           vec_len, c_len, c_pitch);
  /*
  rslt = cudaBindTextureToArray(&allDistsRight2DTex, AC, &TwoDCDesc);
  */
  check_err(rslt, "Unable to bind 2D C texture");

  int inputs = x_len;
  dim3 dim2DBlock(THREADS_PER_DIM, THREADS_PER_DIM);
  dim3 dim2DGrid;
  int cur;
  int niters = 0;
  int offset = 0;
  size_t xo;
  while (inputs > 0) {
    if (inputs > 32768) {
      cur = 32768;
      inputs -= 32768;
    } else {
      cur = inputs;
      inputs = 0;
    }

    
    rslt = cudaBindTexture2D(&xo, allDistsLeft2DTex, devX + offset * (x_pitch / 4),
                             TwoDXDesc, vec_len, cur, x_pitch);
    /*
    rslt = cudaBindTextureToArray(&allDistsLeft2DTex, AX, &TwoDXDesc);
    */
    check_err(rslt, "Unable to bind 2D X texture");

    dim2DGrid = dim3(safe_div(c_len, THREADS_PER_DIM),
                     safe_div(cur, THREADS_PER_DIM));

    all_pairs_dists_kernel<<<dim2DGrid, dim2DBlock>>>
      (cur, c_len, vec_len, devOut + offset * c_len);
      
    offset += cur;
    niters++;
  }

  if (!include_mem) {
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
    pqe = pq_gettime();
  }

  rslt = cudaMemcpy(output, devOut, x_len * c_len * sizeof(float),
                    cudaMemcpyDeviceToHost);
  check_err(rslt, "Unable to copy 2D output to host");

  if (include_mem) {
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
    pqe = pq_gettime();
  }

  
  cudaFree(devX);
  cudaFree(devC);

  /*
  cudaFreeArray(AX);
  cudaFreeArray(AC);
  */
  
  cudaFree(devOut);

  cudaEventElapsedTime(&t, start, stop);
  printf("Wall time for 2D texture: %fms\n", 1000*pq_diff_timers(pqst, pqe));
  printf("GPU time for 2D texture: %fms\n", t);
  printf("Number of iterations for 2D case: %d\n", niters);
  
  printf("checking validity for 2D case\n");
  same = 1;
  for (i = 0; i < x_len; ++i) {
    for (j = 0; j < c_len; ++j) {
      result = 0.0f;
      for (k = 0; k < vec_len; ++k) {
        intermediate = X[i*vec_len + k] - C[j*vec_len + k];
        result += intermediate * intermediate;
      }
      result = sqrt(result);
      if (same && ((fabs(result - output[i*c_len + j]) / result) > tol)) {
        same = 0;
        printf("Different output at X %d, C %d: %f, %f\n", i, j, result,
               output[i*c_len + j]);
      }
    }
  }
  if (same) printf("2D Same!\n");

  cudaFreeHost(X);
  cudaFreeHost(C);
  cudaFreeHost(output);
  cudaEventDestroy(start);
  cudaEventDestroy(stop);

  return 0;
}
