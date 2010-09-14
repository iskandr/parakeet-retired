#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "multi_index_reduce.h"

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
  int vec_len = 32;
  int num_cs = 512;
  int num_vecs = 512;
  int include_mem_in_time = 0;
  int i, j, idx;

  // Process command line
  for (i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "-v")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      vec_len = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-c")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num_cs = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-x")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num_vecs = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-m")) {
      include_mem_in_time = 1;
    }
  }

  float *input;
  float *output;
  int   *valids;
  cudaError_t rslt;
  rslt = cudaMallocHost((void**)&input, num_vecs * vec_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc input: %d\n", rslt);
  rslt = cudaMallocHost((void**)&output, num_cs * vec_len * sizeof(float));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);
  rslt = cudaMallocHost((void**)&valids, num_cs * num_vecs * sizeof(int));
  if (rslt != 0) printf("failed to malloc valids: %d\n", rslt);

  // Initialize arrays
  for (i = 0; i < num_vecs * vec_len; ++i) {
    input[i] = rand() / ((float)rand() + 1);
  }
  for (i = 0; i < num_vecs; ++i) {
    idx = rand() % num_cs;
    for (j = 0; j < num_cs; ++j) {
/*      if (j == idx) {
        valids[num_cs * i + j] = 1;
      } else {
        valids[num_cs * i + j] = 0;
      }*/
      valids[num_cs * i + j] = 1;
    }
  }
  
  multi_index_reduce(input, output, valids, vec_len, num_vecs, num_cs,
                     include_mem_in_time);

  // Check validity
  int same = 1;
  float *sums = (float*)malloc(32 * num_cs * sizeof(float));
  for (i = 0; i < 32 * num_cs; ++i) {
    sums[i] = 0.0f;
  }

  // Compile a list of sums
  int k;
  for (i = 0; i < num_cs; ++i) {
    for (j = 0; j < num_vecs; ++j) {
      if (valids[j * num_cs + i]) {
        for (k = 0; k < 32; ++k) {
          sums[i * 32 + k] += input[j + k];
        }
      }
    }
  }

  // Check the sums against the GPU-computed values
  float diff;
  for (i = 0; i < num_cs; ++i) {
    diff = 0.0f;
    for (j = 0; j < 32; ++j) {
      diff += (sums[i * 32 + j] - output[i * 32 + j]);
    }
    if (same && (diff > 0.1f) || i < 4) {
      for (j = 0; j < 32; ++j) {
        printf("sums[%d;%d]: %f\n", i, j, sums[i * 32 + j]);
        printf("output[%d;%d]: %f\n", i, j, output[i * 32 + j]);
      }
      printf("diff: %f\n", diff);
    }
    same &= (diff <= 0.1f);
  }

  if (same) {
    printf("Same!\n");
  } else {
    printf("Different!\n");
  }

  cudaFreeHost(input);
  cudaFreeHost(output);
  cudaFreeHost(valids);

  return 0;
}

