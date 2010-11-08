#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "base.h"
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
  int vec_len = 512;
  int left_len = 1600;
  int right_len = 1600;
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
  printf("Num left vecs, right vecs, len_vec: %d %d %d\n",
         left_len, right_len, vec_len);
  if (naive) {
    all_dists_naive(left, left_len, right, right_len, vec_len,
                    output, include_mem_in_time);
  } else {
    all_dists_1(left, left_len, right, right_len, vec_len,
                output, include_mem_in_time);
  }

  // Check validity
  int same = 1;
  int left_idx, right_idx, x, y, k;
  float result, intermediate;
  for (y = 0; y < left_len; ++y) {
    for (x = 0; x < right_len; ++x) {
      left_idx  = vec_len * y;
      right_idx = vec_len * x;
      result = intermediate = 0.0f;
      for (k = 0; k < vec_len; ++k) {
        intermediate =
          left[left_idx + k] - right[right_idx + k];
        result += intermediate * intermediate;
      }
      result = sqrt(result);
      if (same &&
          fabs((result - output[y * right_len + x]) / result) >= 0.001f) {
        printf("result[%d;%d]: %f\n", y, x, result);
        printf("output[%d;%d]: %f\n", y, x, output[y * right_len + x]);
        printf("result/output: %f\n\n",
               fabs((result - output[y * right_len + x]) / result));
      }
      same &= ((result - output[y * right_len + x]) / result < 0.001f);
    }
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
