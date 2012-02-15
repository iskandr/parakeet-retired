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

#ifdef __cplusplus
}
#endif
