#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "minall.h"

void usage(void) {
  printf("Usage:\n"
         "--------"
         "-v n       : set length of vectors to be n\n"
         "-x n       : set number of vectors on left hand side to be n\n"
         "-y n       : set number of vectors on right hand side to be n\n"
         "-m         : include memory transfers in timings (default is not to do so)\n"
         "--naive, -n: run the naive kernel (default uses tiling)\n"
         );
}

int main(int argc, char **argv) {
  int vec_len = 512;
  int x_len = 512;
  int y_len = 16000;
  int include_mem_in_time = 0;
  int i;

  // Process command line
  for (i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "-v")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      vec_len = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-x")) {
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
      y_len = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-m")) {
      include_mem_in_time = 1;
    }
  }

  float *input;
  int *output;
  cudaError_t rslt;
  rslt = cudaMallocHost((void**)&input, x_len * y_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc input: %d\n", rslt);
  rslt = cudaMallocHost((void**)&output, x_len * y_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

  // Initialize array
  for (i = 0; i < x_len * y_len; ++i) {
    input[i] = rand() / ((float)rand() + 1);
  }

  minall(input, output, x_len, y_len, include_mem_in_time);

  // Check validity
  int same = 1;
  int j, idx;
  float min = 10e30f;

  for (i = 0; i < y_len; ++i) {
    min = input[i*x_len];
    idx = 0;
    for (j = 1; j < x_len; ++j) {
      if (min > input[i*x_len + j]) {
        min = input[i*x_len + j];
        idx = j;
      }
    }
    if ((same && (output[x_len * i + idx] != 1))) {
      printf("idx for %d: %d\n", i, idx);
      printf("output[%d]: %d\n", i, output[x_len * i + idx]);
      printf("idx used: %d\n", y_len * idx + i);
    }
    for (j = 0; j < x_len; ++j) {
      same &= (output[x_len * i + j] == (j == idx));
    }
  }
  /*
  for (i = 0; i < x_len; ++i) {
    if (i == 65) printf("--");
    for (j = 0; j < y_len; ++j) {
      //if (output[y_len * i + j] != 0)
      printf("%d ", output[y_len * i + j]);
    }
    printf("\n");
  }*/

  if (same) {
    printf("Same!\n");
  } else {
    printf("Different!\n");
  }

  cudaFreeHost(input);
  cudaFreeHost(output);

  return 0;
}

