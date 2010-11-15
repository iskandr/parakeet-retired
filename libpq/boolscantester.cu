/*
 * Parallel Q Library
 *
 * (c) 2009-2010 Eric Hielscher
 *
 * Each-left each-right implementations.
 */

#include <cuda.h>
#include <stdio.h>

#include "scan.cu"

int main(int argc, char **argv) {
  bool input[100000];
  int output[100000];

  int i;
  for (i = 0; i < 100000; ++i) {
    input[i] = (bool)rand() % 2;
  }

  bool *devI;
  int *devO;
  cudaMalloc((void**)&devI, 100000 * sizeof(bool));
  cudaMemcpy(devI, input, 100000 * sizeof(bool), cudaMemcpyHostToDevice);
  cudaMalloc((void**)&devO, 100000 * sizeof(int));

  scan_bool_to_int(devI, 100000, devO);

  cudaMemcpy(output, devO, 100000 * sizeof(int), cudaMemcpyDeviceToHost);

  printf("Size of bool: %d\n", sizeof(bool));
  printf("Size of int:  %d\n", sizeof(int));
  int num = 0;
  int same = 1;
  for (i = 0; i < 100000; ++i) {
    if (input[i]) ++num;
    if (same && output[i] != num) {
      same = 0;
      printf("Different at %d: %d, %d\n", i, num, output[i]);
    }
  }
  if (same) {
    printf("Same!\n");
  }

  cudaFree(devI);
  cudaFree(devO);
}
