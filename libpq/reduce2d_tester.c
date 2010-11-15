#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "base.h"
#include "reduce2d.h"

void usage(void) {
  printf("Usage:\n"
         "--------\n"
         "-l n       : set length of vectors to be n\n"
         "-v n       : set number of vectors on to be n\n"
         "-w n       : set block width to be n\n"
         "-h n       : set block height to be n\n"
         );
}

int main(int argc, char **argv) {
  int vec_len = 512;
  int num_vecs = 64000;
  int bw = 16;
  int bh = 16;
  int i;

  // Process command line
  for (i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "-l")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      vec_len = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-v")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num_vecs = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-w")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      bw = atoi(argv[i]);
      bh = 256 / bw;
    } else if (!strcmp(argv[i], "-h")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      bh = atoi(argv[i]);
      bw = 256 / bh;
    }
  }

  int *vecs, *output;
  cudaError_t rslt;
  rslt = cudaMallocHost((void**)&vecs, num_vecs * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc vecs: %d\n", rslt);
  rslt = cudaMallocHost((void**)&output, vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

  // Initialize arrays
  for (i = 0; i < num_vecs * vec_len; ++i) {
    vecs[i] = rand() % 20;
  }

  // Call kernel
  launch_reduce2d(vecs, num_vecs, vec_len, output, bw, bh);

  // Check validity
  int *real = (int*)malloc(sizeof(int) * vec_len);
  memset(real, 0, sizeof(int) * vec_len);
  int j;
  struct timeval *start, *end;
  start = pq_gettime();
  for (i = 0; i < num_vecs; ++i) {
    for (j = 0; j < vec_len; ++j) {
      real[j] += vecs[i*vec_len + j];
    }
  }
  end = pq_gettime();
  printf("CPU time to reduce: %f\n", 1000 * pq_diff_timers(start, end));
    
  int same = 1;
  for (i = 0; i < vec_len; ++i) {
    if (same && real[i] != output[i]) {
      printf("Different at %d, with real,out = %d,%d\n", i, real[i], output[i]);
      same = 0;
    }
  }

  if (same) {
    printf("Same!\n");
  } else {
    printf("Different!\n");
  }

  free(real);
  cudaFreeHost(vecs);
  cudaFreeHost(output);

  return 0;
}
