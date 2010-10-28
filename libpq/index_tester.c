#include <cuda_runtime_api.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "index.h"

void usage(void) {
  printf("Usage:\n"
         "--------"
         "-l n       : set length of vectors to be n\n"
         "-v n       : set number of vectors on to be n\n"
         "-i n       : set number of vectors indexes to be n\n"
         "--naive, -n: run the naive kernel (default uses tiling)\n"
         );
}

int main(int argc, char **argv) {
  int vec_len = 512;
  int num_vecs = 64000;
  int num_idxs = 256;
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
      vec_len = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-v")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num_vecs = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-i")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num_idxs = atoi(argv[i]);
    } else if (!strcmp(argv[i], "--naive") ||
               !strcmp(argv[i], "-n")) {
      naive = 1;
    }
  }

  int *vecs, *idxs, *output;
  cudaError_t rslt;
  rslt = cudaMallocHost((void**)&vecs, num_vecs * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc vecs: %d\n", rslt);
  rslt = cudaMallocHost((void**)&idxs, num_idxs * sizeof(int));
  if (rslt != 0) printf("failed to malloc idxs: %d\n", rslt);
  rslt = cudaMallocHost((void**)&output, num_idxs * vec_len * sizeof(int));
  if (rslt != 0) printf("failed to malloc output: %d\n", rslt);

  // Initialize arrays
  for (i = 0; i < num_vecs * vec_len; ++i) {
    vecs[i] = rand() % 20;
  }
  int compaction_factor = num_vecs / num_idxs;
  int cur_idx = 0;
  for (i = 0; i < num_idxs; ++i) {
    cur_idx += 1 + rand() % compaction_factor;
    idxs[i] = cur_idx;
  }

  // Call kernel
  launch_index(vecs, num_vecs, idxs, num_idxs, vec_len, output);

  // Check validity
  int j;
  int same = 1, mark_same = 0;
  for (i = 0; i < num_idxs; ++i) {
    for (j = 0; j < vec_len; ++j) {
      if (vecs[idxs[i]*vec_len + j] != output[i*vec_len + j] && same) {
        printf("Different at (%d,%d), with idx,real,out = %d,%d,%d\n",
               i, j, idxs[i], vecs[idxs[i]*vec_len + j], output[i*vec_len + j]);
        mark_same = 1;
      }
    }
    if (mark_same) {
      mark_same = same = 0;
    }
  }

  if (same) {
    printf("Same!\n");
  } else {
    printf("Different!\n");
  }

  cudaFreeHost(vecs);
  cudaFreeHost(idxs);
  cudaFreeHost(output);

  return 0;
}
