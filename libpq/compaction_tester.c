#include <cuda.h>
#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "base.h"
#include "gen_idx.h"
#include "index.h"
#include "scan.h"
#include "where.h"

void usage(void) {
  printf("Usage:\n"
         "--------"
         "-l n       : set length of vectors to be l\n"
         "-v n       : set number of vectors to be v\n"
         "-b n       : set number of bins to be n\n"
         "-m         : include memory allocs & transfers in timing\n"
         );
}

int main(int argc, char **argv) {
  int vec_len = 512;
  int num_vecs = 65536;
  int num_bins = 20;
  int memtime = 0;
  int i;

  // Process command line
  for (i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "-l")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      vec_len = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-v")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num_vecs = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-b")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num_bins = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-m")) {
      memtime = 1;
    }
  }

  // Get device info
  size_t fm, tm;
  cudaMemGetInfo(&fm, &tm);
  printf("Mem info: %d free, %d total\n", fm, tm);

  // Alloc memory for host data, do some setup
  int *vecs, *a, *output;
  cudaError_t rslt;
  cudaEvent_t start, stop;
  struct timeval *ts_start, *ts_end;
  float t;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  rslt = cudaMallocHost((void**)&vecs, num_vecs * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc vecs: %d\n", rslt);
  rslt = cudaMallocHost((void**)&a, num_vecs * sizeof(int));
  if (rslt != 0) printf("failed to malloc assignment: %d\n", rslt);

  // Initialize arrays
  for (i = 0; i < num_vecs * vec_len; ++i) {
    vecs[i] = rand() % 20;
  }
  for (i = 0; i < num_vecs; ++i) {
    a[i] = rand() % num_bins;
  }

  if (memtime) {
    cudaEventRecord(start, 0);
    cudaEventSynchronize(start);
    ts_start = pq_gettime();
  }
  
  // Copy data from host to device
  int *devVecs, *devA, *devPs, *devIdxs, *devOutput;
  rslt = cudaMalloc((void**)&devVecs, num_vecs * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Vecs: %d\n", rslt);
  rslt = cudaMalloc((void**)&devA, num_vecs * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev A: %d\n", rslt);
  rslt = cudaMalloc((void**)&devPs, num_vecs * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Ps: %d\n", rslt);
  rslt = cudaMemcpy(devVecs, vecs, num_vecs * vec_len * sizeof(int),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy vecs to device: %d\n", rslt);
  rslt = cudaMemcpy(devA, a, num_vecs * sizeof(int), cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy A to device: %d\n", rslt);

  if (!memtime) {
    cudaEventRecord(start, 0);
    cudaEventSynchronize(start);
    ts_start = pq_gettime();
  }
  
  // Generate the "where" binary vector
  launch_where_dev(0, devA, num_vecs, devPs, 1);
  
  // Prefix sum the binary vector
  scan_int(devPs, num_vecs, devPs, 1);

  // Get the value of the prefix sum so as to decide how much output space to
  // allocate
  int num_idxs;
  rslt = cudaMemcpy(&num_idxs, devPs + num_vecs - 1, sizeof(int),
                    cudaMemcpyDeviceToHost);
  if (rslt != 0) printf("failed to get prefix sum: %d\n", rslt);
  printf("Number of indexes: %d\n", num_idxs);

  // Allocate the indexes and output vectors on the device
  rslt = cudaMalloc((void**)&devIdxs, num_idxs * sizeof(int));
  if (rslt != 0) printf("failed to malloc idxs: %d\n", rslt);
  rslt = cudaMalloc((void**)&devOutput, num_idxs * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

  // Generate the indexes from the prefix sum
  launch_gen_index_dev(devPs, num_vecs, devIdxs, 1);

  // Perform the indexing
  launch_index_dev(devVecs, num_vecs, devIdxs, num_idxs, vec_len, devOutput, 1);

  if (!memtime) {
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
    ts_end = pq_gettime();
  }

  // Copy result back to host
  rslt = cudaMallocHost((void**)&output, num_idxs * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);
  rslt = cudaMemcpy(output, devOutput, num_idxs * vec_len * sizeof(int),
                    cudaMemcpyDeviceToHost);
  if (rslt != 0) printf("failed to copy output to host: %d\n", rslt);

  if (memtime) {
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
    ts_end = pq_gettime();
  }

  cudaEventElapsedTime(&t, start, stop);
  printf("Time for kernels: %f\n", t);
  cudaEventDestroy(start);
  cudaEventDestroy(stop);
  printf("Wall time for kernels: %f\n", pq_diff_timers(ts_start, ts_end));
  
  // Check validity
  int same = 1;
  int *ps = (int*)malloc(num_vecs * sizeof(int));
  cudaMemcpy(ps, devPs, num_vecs * sizeof(int), cudaMemcpyDeviceToHost);
  int sum = 0;
  for (i = 0; i < num_vecs; ++i) {
    if (a[i] == 0) ++sum;
    if (same && ps[i] != sum) {
      printf("prefix sum different at idx %d, (sum,ps[i]): (%d,%d)\n", i,
             sum, ps[i]);
      same = 0;
    }
  }
  if (same) printf("Prefix sum same!\n");
  free(ps);
  int *idxs = (int*)malloc(num_idxs * sizeof(int));
  same = 1;
  cudaMemcpy(idxs, devIdxs, num_idxs * sizeof(int), cudaMemcpyDeviceToHost);
  int num = 0;
  for (i = 0; i < num_vecs; ++i) {
    if (a[i] == 0) {
      if (same && idxs[num] != i) {
        same = 0;
        printf("idxs wrong at pos %d: %d\n", i, idxs[num]);
      }
      ++num;
    } 
  }
  if (same) {
    printf("idxs same!\n");
  }
  free(idxs);
  
  int j;
  int mark_same = 0, pos = 0;
  same = 1;
  for (i = 0; i < num_vecs; ++i) {
    if (a[i] == 0) {
      for (j = 0; j < vec_len; ++j) {
        if (vecs[i*vec_len + j] != output[pos*vec_len + j] && same) {
          printf("Different at (%d,%d), with a,real,out = %d,%d,%d\n",
                i, j, a[i], vecs[i*vec_len + j], output[pos*vec_len + j]);
          same = 0;
        }
      }
      ++pos;
    }
  }
  printf("pos: %d\n", pos);

  if (same) {
    printf("Same!\n");
  } else {
    printf("Different!\n");
  }

  cudaFree(devVecs);
  cudaFree(devA);
  cudaFree(devPs);
  cudaFree(devIdxs);
  cudaFree(devOutput);
  cudaFreeHost(vecs);
  cudaFreeHost(a);
  cudaFreeHost(output);

  return 0;
}
