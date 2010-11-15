#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>

#include "base.h"

void usage(void) {
  printf("Usage:\n"
         "--------\n"
         "-l n       : set length of vectors to be n\n"
         );
}

int main(int argc, char **argv) {
  int vec_len = 1000;
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
    }
  }

  int *x, *y, *output;
  x = (int*)malloc(vec_len * sizeof(int));
  y = (int*)malloc(vec_len * sizeof(int));
  output = (int*)malloc(vec_len * vec_len * sizeof(int));

  struct timeval *start, *end;
  int j;

  start = pq_gettime();
  for (i = 0; i < vec_len; ++i) {
    for (j = 0; j < vec_len; ++j) {
      output[i*vec_len + j] = x[i] - y[j];
    }
  }
  end = pq_gettime();

  printf("Total time: %f\n", pq_diff_timers(start, end));

  free(x);
  free(y);
  free(output);

  return 0;
}