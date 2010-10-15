#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "reduce.h"

void usage(void) {
  printf("Usage:\n"
         "--------"
         "-x n       : set number of vectors on left hand side to be n\n"
         );
}

int main(int argc, char **argv) {
  int num = 100000;
  int i;

  // Process command line
  for (i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "-x")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num = atoi(argv[i]);
    }
  }

  int *input;
  int output;
  cudaError_t rslt;
  rslt = cudaMallocHost((void**)&input, num * sizeof(int));
  if (rslt != 0) printf("failed to malloc input: %d\n", rslt);

  // Initialize array
  for (i = 0; i < num; ++i) {
    input[i] = rand() % 10;
  }

  reduce(input, &output, num);

  // Check validity
  int localTotal = 0;
  for (i = 0; i < num; ++i) {
    localTotal += input[i];
  }

  if (localTotal == output) {
    printf("Same!\n");
  } else {
    printf("Different!\n");
    printf("Local: %d, GPU: %d\n", localTotal, output);
  }

  cudaFreeHost(input);

  return 0;
}
