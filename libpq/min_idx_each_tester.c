#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "min_idx_each.h"

void usage(void) {
  printf("Usage:\n"
         "--------"
         "-x n       : set number of vectors\n"
         "-y n       : set number of centroids\n"
         );
}

int main(int argc, char **argv) {
  int num_rows = 10000;
  int num_cols = 100;
  int i;

  // Process command line
  for (i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "-x")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num_rows = atoi(argv[i]);
    }
    if (!strcmp(argv[i], "-y")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num_cols = atoi(argv[i]);
    }
  }

  int *input;
  int *output;
  cudaError_t rslt;
  printf("Allocating host mem\n");
  rslt = cudaMallocHost((void**)&input, num_rows * num_cols * sizeof(int));
  if (rslt != 0) printf("failed to malloc input: %d\n", rslt);
  rslt = cudaMallocHost((void**)&output, num_rows * sizeof(int));
  if (rslt != 0) printf("failed to malloc input: %d\n", rslt);

  // Initialize array
  printf("initing host input\n");
  for (i = 0; i < num_rows * num_cols; ++i) {
    input[i] = rand() % 10;
  }

  printf("launching kernel\n");
  min_idx_each_rowmajor(input, output, num_rows, num_cols);

  // Check validity
  int min, min_idx;
  int same = 1;
  int j, offset;
  printf("checking validity\n");
  /*
  for (i = 0; i < num_rows; ++i) {
    min = input[i*num_cols];
    min_idx = 0;
    for (j = 1; j < num_cols; ++j) {
      if (min > input[i*num_cols + j]) {
        min = input[i*num_cols + j];
        min_idx = j;
      }
    }
    if (output[i] != min_idx) {
      printf("Different on idx %d\n", i);
      same = 0;
    }
  }
  */

  for (i = 0; i < num_rows; ++i) {
    min = input[i];
    min_idx = 0;
    offset = 0;
    for (j = 1; j < num_cols; ++j) {
      offset += num_rows;
      if (min > input[i + j*num_rows]) {
        min = input[i + j*num_rows];
        min_idx = j;
      }
    }
    if (output[i] != min_idx && same) {
      printf("Different on idx %d, vals (%d,%d)\n", i, min_idx, output[i]);
      same = 0;
    }
  }
  
  if (same) printf("Same!\n");
  
  cudaFreeHost(input);
  cudaFreeHost(output);

  return 0;
}
