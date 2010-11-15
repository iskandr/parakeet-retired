#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gen_idx.h"

void usage(void) {
  printf("Usage:\n"
         "--------"
         "-i n       : set number of input idxs to be n\n"
         "-v n       : set number of valid idxs to be n\n"
         );
}

int main(int argc, char **argv) {
  int num_input = 33554432; //16777216;
  int num_idxs = 8388608; //1048576;
  int i;

  // Process command line
  for (i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "-i")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num_input = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-v")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num_idxs = atoi(argv[i]);
    }
  }

  int *input, *output;
  cudaError_t rslt;
  rslt = cudaMallocHost((void**)&input, num_input * sizeof(int));
  if (rslt != 0) printf("failed to malloc input: %d\n", rslt);
  rslt = cudaMallocHost((void**)&output, num_idxs * sizeof(int));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

  // Initialize array
  // TODO: Pretty uniform here, maybe check performance on less uniform inputs
  int fac = num_input / num_idxs;
  int idx, j;
  for (i = 0; i < num_idxs; ++i) {
    idx = rand() % fac;
    for (j = 0; j < idx; ++j) {
      input[i*fac + j] = i;
    }
    for (j = idx; j < fac; ++j) {
      input[i*fac + j] = i+1;
    }
  }

  // Call kernel
  launch_gen_index(input, num_input, output);

  // Check validity
  int same = 1;
  if (input[0] == 1) {
    if (output[0] != 0) {
      printf("Different at idx 0, input, output = %d,%d\n", 0, output[0]);
      same = 0;
    }
  }
  for (i = 1; i < num_input; ++i) {
    if (input[i] != input[i-1]) {
      if (same && output[input[i-1]] != i) {
        printf("Different at input idx %d; output = %d\n",
               i, output[input[i-1]]);
        same = 0;
      }
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
