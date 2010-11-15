#include <stdlib.h>
#include <sys/time.h>
#include <time.h>

#include "base.h"

void usage(void) {
  printf("Usage:\n"
         "--------\n"
         "-l n       : set length of vectors to be n\n"
         );
}

typedef int v4si __attribute__ ((mode(V4SI)));
union i4vec {
  v4si v;
  int i[4];
};

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

  if (vec_len % 4 != 0) {
    printf("Vec len must be multiple of 4. Aborting.\n");
    exit(0);
  }

  i4vec *x, *y, *output;
  x = (i4vec*)malloc(sizeof(i4vec) * vec_len / 4);
  y = (i4vec*)malloc(sizeof(i4vec) * vec_len / 4);
  output = (i4vec*)malloc(sizeof(i4vec) * (vec_len / 4) * (vec_len / 4));
  
  for (i = 0; i < vec_len / 4; ++i) {
    x[i].i[0] = rand() % 20;
    x[i].i[1] = rand() % 20;
    x[i].i[2] = rand() % 20;
    x[i].i[3] = rand() % 20;
    y[i].i[0] = rand() % 20;
    y[i].i[1] = rand() % 20;
    y[i].i[2] = rand() % 20;
    y[i].i[3] = rand() % 20;
  }
  
  struct timeval *start, *end;
  int j;
  start = pq_gettime();

  for (i = 0; i < vec_len / 4; ++i) {
    for (j = 0; j < vec_len / 4; ++j) {
      