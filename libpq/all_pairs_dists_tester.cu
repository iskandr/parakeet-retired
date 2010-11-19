#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "all_pairs_dists_1dtex_kernel.cu"
#include "all_pairs_dists_notex_kernel.cu"
#include "base.h"

const int chunklen = 32768;
const int minfree = 50000000;

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
  rslt = cudaHostAlloc((void**)&X, x_len * vec_len * sizeof(int),
                       cudaHostAllocPortable);
  if (rslt != 0) printf("failed to malloc X: %d\n", rslt);
  rslt = cudaHostAlloc((void**)&C, c_len * vec_len * sizeof(int),
                       cudaHostAllocPortable);
  if (rslt != 0) printf("failed to malloc C: %d\n", rslt);
  rslt = cudaHostAlloc((void**)&output, x_len * c_len * sizeof(float),
                       cudaHostAllocPortable);
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

  /** 2D Texturing **/

  cudaStream_t stream[2];
  cudaStreamCreate(&stream[0]);
  cudaStreamCreate(&stream[1]);

  float gputime = 0.0f;
  float walltime = 0.0f;
  float tmptime;

  if (include_mem) {
    cudaEventRecord(start, 0);
    pqst = pq_gettime();
  }

  cudaChannelFormatDesc TwoDXDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  cudaChannelFormatDesc TwoDCDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);

  /** Try out my own "pitch" **/
  int mypitch = 8;
  while (mypitch < vec_len) {
    mypitch += 8;
  }
  
  rslt = cudaMalloc((void**)&devOut, x_len * c_len * sizeof(float));
  check_err(rslt, "Unable to malloc devOut");

  rslt = cudaMalloc((void**)&devC, mypitch * c_len * sizeof(int));
  check_err(rslt, "Unable to malloc devC");

  rslt = cudaMemcpy2D(devC, mypitch * sizeof(int), C, vec_len * sizeof(int),
                      vec_len * sizeof(int), c_len, cudaMemcpyHostToDevice);
  check_err(rslt, "Unable to copy C to device");
  
  size_t free_mem, total;
  cudaMemGetInfo(&free_mem, &total);
  printf("Free and total mem on card: %d, %d\n", (int)free_mem, (int)total);
  
  int xbytes = x_len * mypitch * sizeof(int);

  // For now, assume that x >> c
  int *devX2;
  int num_xfers = 1;
  int rows_per_xfer = x_len;
  int texchunksperxfer = safe_div(x_len, chunklen);
  int chunked = 0;

  if (xbytes > free_mem || free_mem - xbytes < minfree) {
    chunked = 1;
    
    texchunksperxfer =
      (free_mem - minfree) / (chunklen * mypitch * sizeof(int) * 2);
    rslt = cudaMalloc((void**)&devX,
                      mypitch * texchunksperxfer * chunklen * sizeof(int));
    check_err(rslt, "Unable to malloc devX chunk 1");
    rslt = cudaMalloc((void**)&devX2,
                      mypitch * texchunksperxfer * chunklen * sizeof(int));
    check_err(rslt, "Unable to malloc devX chunk 2");
    /*
    texchunksperxfer =
      (free_mem - minfree) / (chunklen * mypitch * sizeof(int));
    rslt = cudaMalloc((void**)&devX,
                      mypitch * texchunksperxfer * chunklen * sizeof(int));
    */

    rows_per_xfer = chunklen * texchunksperxfer;
    num_xfers = safe_div(x_len, rows_per_xfer);
  } else {
    rslt = cudaMalloc((void**)&devX, mypitch * x_len * sizeof(int));
    check_err(rslt, "Unable to malloc non-chunked devX");
  }
  size_t x_pitch = mypitch * 4;
  size_t c_pitch = mypitch * 4;

  cudaMemGetInfo(&free_mem, &total);
  printf("GPU free after inputs allocated: %d\n", (int)free_mem);
  
  rslt = cudaBindTexture2D(0, allDistsRight2DTex, devC, TwoDCDesc,
                           vec_len, c_len, c_pitch);
  check_err(rslt, "Unable to bind 2D C texture");

  int first = 1;
  int *tmp;
  int rowoffset = 0;
  int cur = chunklen;
  int num_k_groups = num_xfers;
  int numrowstocopy = rows_per_xfer;
  dim3 dim2DGrid, dim2DBlock;

  dim2DBlock = dim3(THREADS_PER_DIM, THREADS_PER_DIM);

  while(num_k_groups > 0) {
    
    // Swap chunks, or copy down first chunk
    if (first) {
      rslt = cudaMemcpy2DAsync(devX, x_pitch, X, vec_len * sizeof(int),
                               vec_len * sizeof(int), rows_per_xfer,
                               cudaMemcpyHostToDevice, stream[0]);
      check_err(rslt, "Unable to copy first chunk down");
      first = 0;
      num_xfers--;
    } else {
      cudaStreamSynchronize(stream[0]);
      cudaStreamSynchronize(stream[1]);
      tmp = devX;
      devX = devX2;
      devX2 = tmp;
    }

    // Asynchronously copy to other chunk
    // TODO: Is this right?
    if (num_xfers == 1) numrowstocopy = x_len - (rowoffset + rows_per_xfer);
    if (num_xfers > 0) {
      rslt = cudaMemcpy2DAsync(devX2,
                               x_pitch,
                               X + (rowoffset + rows_per_xfer) * vec_len,
                               vec_len * sizeof(int),
                               vec_len * sizeof(int),
                               numrowstocopy,
                               cudaMemcpyHostToDevice,
                               stream[1]);
      check_err(rslt, "Unable to copy asynchronous chunk on stream 1");
      num_xfers--;
    }

    /*
    if (num_xfers == 1) numrowstocopy = x_len - rowoffset;
    cudaMemcpy2D(devX, x_pitch, X + rowoffset * vec_len,
                 vec_len * sizeof(int), vec_len * sizeof(int),
                 numrowstocopy, cudaMemcpyHostToDevice);
    num_xfers--;

    if (!include_mem && first) {
      cudaEventRecord(start, 0);
      pqst = pq_gettime();
      first = 0;
    }
    */

    // Asynchronously launch kernel(s)
    if (!include_mem) {
      cudaEventRecord(start, stream[0]);
      cudaEventSynchronize(start);
      pqst = pq_gettime();
    }
    
    for (j = 0; j < texchunksperxfer && rowoffset < x_len; ++j) {
      if (rowoffset + chunklen > x_len) cur = x_len - rowoffset;
      rslt = cudaBindTexture2D(0, allDistsLeft2DTex,
                               devX + j * chunklen * (x_pitch / 4),
                               TwoDXDesc, vec_len, cur, x_pitch);
      check_err(rslt, "Unable to bind X texture");

      dim2DGrid = dim3(safe_div(c_len, THREADS_PER_DIM),
                       safe_div(cur, THREADS_PER_DIM));

      all_pairs_dists_kernel<<<dim2DGrid, dim2DBlock, stream[0]>>>
      //all_pairs_dists_kernel<<<dim2DGrid, dim2DBlock>>>
        (cur, c_len, vec_len, devOut + rowoffset * c_len);
      check_err(cudaGetLastError(), "Error launching kernel");
      
      rowoffset += chunklen;
    }
    num_k_groups--;

    if (!include_mem) {
      cudaEventRecord(stop, stream[0]);
      cudaEventSynchronize(stop);
      pqe = pq_gettime();

      cudaEventElapsedTime(&tmptime, start, stop);
      gputime += tmptime;
      walltime += 1000*pq_diff_timers(pqst, pqe);
    }
  }

  
  cudaStreamSynchronize(stream[0]);
  cudaStreamSynchronize(stream[1]);
  cudaStreamDestroy(stream[0]);
  cudaStreamDestroy(stream[1]);

  rslt = cudaMemcpy(output, devOut, x_len * c_len * sizeof(float),
                    cudaMemcpyDeviceToHost);
  check_err(rslt, "Unable to copy 2D output to host");

  if (include_mem) {
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
    pqe = pq_gettime();

    cudaEventElapsedTime(&gputime, start, stop);
    walltime = 1000*pq_diff_timers(pqst, pqe);
  }

  
  cudaFree(devX);
  cudaFree(devC);
  cudaFree(devOut);

  printf("Wall time for 2D texture: %fms\n", walltime);
  printf("GPU time for 2D texture: %fms\n", gputime);
  num_xfers = safe_div(x_len, chunklen * texchunksperxfer);
  if (chunked) {
    printf("Number of mem transfers for 2D case: %d\n", num_xfers);
    printf("Number of texture bindings: %d\n", num_xfers * texchunksperxfer);
  } else {
    printf("Number of texture bindings: %d\n", safe_div(x_len, chunklen));
  }

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
