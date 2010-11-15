#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "where.h"

void usage(void) {
  printf("Usage:\n"
         "--------"
         "-l n       : set length of vector to be whered\n"
         "-i n       : set number of matches\n"
         "--naive, -n: run the naive kernel (default uses tiling)\n"
         );
}

int main(int argc, char **argv) {
  int input_len = 4194304;
  int num_matches = 16384;
  int naive = 0;
  int i;

  // Process command line
  for (i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "-l")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      input_len = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-i")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num_matches = atoi(argv[i]);
    } else if (!strcmp(argv[i], "--naive") ||
               !strcmp(argv[i], "-n")) {
      naive = 1;
    }
  }

  int *input, *output;
  cudaError_t rslt;
  rslt = cudaMallocHost((void**)&input, input_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc idxs: %d\n", rslt);
  rslt = cudaMallocHost((void**)&output, input_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

  // Initialize arrays
  int fac = input_len / num_matches;
  for (i = 0; i < input_len; ++i) {
    input[i] = rand() % fac;
  }

  // Call kernel
  launch_where(0, input, input_len, output);

  // Check validity
  int same = 1;
  for (i = 0; i < input_len; ++i) {
    if (same && ((input[i] == 0 && output[i] != 1) ||
                 (input[i] != 0 && output[i] == 1))) {
      printf("Different at index %d: %d, %d\n", i, input[i], output[i]);
      same = 0;
    }
  }

  if (same) {
    printf("Same!\n");
  } else {
    printf("Different!\n");
  }

  cudaFreeHost(input);
  cudaFreeHost(output);

  return 0;
}
