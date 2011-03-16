#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <time.h>

#include "base.h"

#ifdef __cplusplus
extern "C" {
#endif

void check_err(int rslt, char *msg) {
  if (rslt != 0) {
    printf("%s: %d\n", msg, rslt);
    exit(1);
  }
}

int safe_div(int n, int d) {
  return (n + d - 1) / d;
}

void make_linear_grid(int num_blocks, int *gridX, int *gridY) {
  if (num_blocks > 16384) {
    *gridX = 16384;
    *gridY = safe_div(num_blocks, 16384);
  } else {
    *gridX = num_blocks;
    *gridY = 1;
  }
}

double pq_diff_timers(struct timeval *start, struct timeval *end) {
  double ret;

  if (end->tv_usec < start->tv_usec) {
    int nsec = (start->tv_usec - end->tv_usec) / 1000000 + 1;
    start->tv_usec -= 1000000 * nsec;
    start->tv_sec += nsec;
  }
  if (end->tv_usec - start->tv_usec > 1000000) {
    int nsec = (end->tv_usec - start->tv_usec) / 1000000;
    start->tv_usec += 1000000 * nsec;
    start->tv_sec -= nsec;
  }

  ret = (end->tv_sec - start->tv_sec) +
        (end->tv_usec - start->tv_usec) / 1000000.0;

  free(start);
  free(end);

  return ret;
}

struct timeval *pq_gettime(void) {
  struct timeval *ret = (struct timeval*)(malloc(sizeof(struct timeval)));
  gettimeofday(ret, NULL);
  return ret;
}

#ifdef __cplusplus
}
#endif
