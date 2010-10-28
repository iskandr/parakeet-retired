#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "all_pairs_abs_diff.h"

void usage(void) {
  printf("Usage:\n"
         "--------"
         "-x n       : set number of vectors\n"
         "-y n       : set number of centroids\n"
         "-l n       : set vector length\n"
         );
}

int main(int argc, char **argv) {
  int left_len = 4096;
  int right_len = 128;
  int vec_len = 128;
  int i;

  // Process command line
  for (i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "-x")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      left_len = atoi(argv[i]);
    }
    if (!strcmp(argv[i], "-y")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      right_len = atoi(argv[i]);
    }
    if (!strcmp(argv[i], "-l")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      vec_len = atoi(argv[i]);
    }
  }

  int *X;
  int *C;
  int *output;
  cudaError_t rslt;
  printf("Allocating host mem\n");
  rslt = cudaMallocHost((void**)&X, left_len * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc X: %d\n", rslt);
  rslt = cudaMallocHost((void**)&C, right_len * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc X: %d\n", rslt);
  rslt = cudaMallocHost((void**)&output, left_len * right_len * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

  // Initialize array
  printf("initing host input\n");
  for (i = 0; i < left_len * vec_len; ++i) {
    X[i] = rand() % 10;
  }
  for (i = 0; i < right_len * vec_len; ++i) {
    C[i] = rand() % 10;
  }

  printf("launching kernel\n");
  all_pairs_abs_diff(X, left_len, C, right_len, vec_len, output);

  // Check validity
  int same = 1;
  int j, k;
  printf("checking validity\n");

  for (i = 0; i < left_len; ++i) {
    for (j = 0; j < right_len; ++j) {
      for (k = 0; k < vec_len; ++k) {
        if (same && abs(X[vec_len*i + k] - C[vec_len*j + k]) !=
            output[i*right_len*vec_len + j*vec_len + k]) {
          same = 0;
          printf("Different at idxes (%d,%d), vals cpu/gpu: (%d,%d)\n",
                 i, j, abs(X[vec_len*i + k] - C[vec_len*j + k]),
                 output[i*right_len*vec_len + j*vec_len + k]);
        }
      }
    }
  }

  /*
  printf("X:\n");
  for (i = 0; i < left_len; ++i) {
    for (k = 0; k < vec_len; ++k) {
      printf("%3d ", X[vec_len*i + k]);
    }
    printf("\n");
  }
  printf("Y:\n");
  for (j = 0; j < right_len; ++j) {
    for (k = 0; k < vec_len; ++k) {
      printf("%3d ", C[vec_len*j + k]);
    }
    printf("\n");
  }
  printf("GPU diff:\n");
  for (i = 0; i < left_len; ++i) {
    printf("X[%d]:\n\n", i);
    for (j = 0; j < right_len; ++j) {
      printf("C[%d]:\n\n", j);
      for (k = 0; k < vec_len; ++k) {
        printf("%3d ", output[i*right_len*vec_len + j*vec_len + k]);
      }
      printf("\n");
    }
  }
  */

  if (same) printf("Same!\n");

  cudaFreeHost(X);
  cudaFreeHost(C);
  cudaFreeHost(output);

  return 0;
}
