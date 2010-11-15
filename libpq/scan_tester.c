#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "scan.h"

void usage(void) {
  printf("Usage:\n"
         "--------"
         "-l n       : set length of vector to be scanned\n"
         );
}

int main(int argc, char **argv) {
  int input_len = 4194304;
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
    }
  }

  int *input, *output;
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);
  rslt = cudaMallocHost((void**)&input, input_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc idxs: %d\n", rslt);
  rslt = cudaMallocHost((void**)&output, input_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

  // Initialize arrays
  for (i = 0; i < input_len; ++i) {
    input[i] = rand() % 10;
  }

  // Copy Data to Device
  int *devInput, *devOutput;
  rslt = cudaMalloc((void**)&devInput, input_len * sizeof(int));
  rslt = cudaMemcpy(devInput, input, input_len * sizeof(int),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy dev Input: %d\n", rslt);
  rslt = cudaMalloc((void**)&devOutput, input_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Output: %d\n", rslt);

  // Call kernel
  cudaEventRecord(start, 0);
  scan_int(devInput, input_len, devOutput);
  cudaEventRecord(stop, 0);
  cudaEventSynchronize(stop);
  
  // Copy Data to Host
  rslt = cudaMemcpy(output, devOutput, sizeof(int) * input_len,
                    cudaMemcpyDeviceToHost);
  if (rslt != 0) printf("failed to copy dev Output: %d\n", rslt);

  float t;
  cudaEventElapsedTime(&t, start, stop);
  printf("Time for kernel: %f\n", t);
  cudaEventDestroy(start);
  cudaEventDestroy(stop);

  // Check validity
  int same = 1, sum = 0;
  for (i = 0; i < input_len; ++i) {
    sum += input[i];
    if (same && output[i] != sum) {
      printf("Different at index %d: %d, %d\n", i, sum, output[i]);
      same = 0;
    }
  }

  if (same) {
    printf("Same!\n");
  } else {
    printf("Different!\n");
  }

  cudaFree(devInput);
  cudaFree(devOutput);
  cudaFreeHost(input);
  cudaFreeHost(output);

  return 0;
}
