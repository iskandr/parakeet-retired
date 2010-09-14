#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "each.h"

void usage(void) {
  printf("Usage:\n"
         "--------"
         "-v n       : set length of vectors to be n\n"
         "-l n       : set number of vectors on left hand side to be n\n"
         "-r n       : set number of vectors on right hand side to be n\n"
         "-m         : include memory transfers in timings (default is not to do so)\n"
         "--naive, -n: run the naive kernel (default uses tiling)\n"
         );
}

int main(int argc, char **argv) {
  int vec_len = 16;
  int left_len = 6016;
  int right_len = 6016;
  int include_mem_in_time = 0;
  int naive = 0;
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
    } else if (!strcmp(argv[i], "-l")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      left_len = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-r")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      right_len = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-m")) {
      include_mem_in_time = 1;
    } else if (!strcmp(argv[i], "--naive") ||
               !strcmp(argv[i], "-n")) {
      naive = 1;
    }
  }

  float *left, *right, *output;
  cudaError_t rslt;
  rslt = cudaMallocHost((void**)&left, left_len * vec_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc left: %d\n", rslt);
  rslt = cudaMallocHost((void**)&right, right_len * vec_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc right: %d\n", rslt);
  rslt = cudaMallocHost((void**)&output, left_len * right_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

  // Initialize arrays
  for (i = 0; i < left_len * vec_len; ++i) {
    left[i] = rand() / ((float)rand() + 1);
  }
  for (i = 0; i < right_len * vec_len; ++i) {
    right[i] = rand() / ((float)rand() + 1);
  }

  // Call all-dists
  if (naive) {
    all_dists_naive(left, left_len, right, right_len, vec_len,
                    output, include_mem_in_time);
  } else {
    all_dists_1(left, left_len, right, right_len, vec_len,
                output, include_mem_in_time);
  }

  // Check validity
  int same = 1;
  int left_idx, right_idx, j;
  float result, intermediate;
  for (i = 0; i < left_len * right_len; ++i) {
    left_idx  = i / right_len;
    right_idx = i - (right_len * left_idx);
    result = intermediate = 0.0f;
    for (j = 0; j < vec_len; ++j) {
      intermediate =
        left[left_idx * vec_len + j] - right[right_idx * vec_len + j];
      result += intermediate * intermediate;
    }
    result = sqrt(result);
    if ((same && (fabs(1.0f - result / output[i]) >= 0.001f)) || i < 10) {
      printf("result[%d]: %f\n", i, result);
      printf("output[%d]: %f\n", i, output[i]);
      printf("result/output: %f\n\n", fabs(1.0f - result / output[i]));
    }
    same &= (fabs(1.0f - result / output[i]) < 0.001f);
  }

  if (same) {
    printf("Same!\n");
  } else {
    printf("Different!\n");
  }

  cudaFreeHost(left);
  cudaFreeHost(right);
  cudaFreeHost(output);

  return 0;
}

