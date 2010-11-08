#ifndef _BASE_H_
#define _BASE_H_

#ifdef __cplusplus
extern "C" {
#endif

#define THREADS_PER_LINEAR_BLOCK 256

void check_err(int rslt, char *msg);
void make_linear_grid(int num_blocks, int *gridX, int *gridY);
int safe_div(int n, int d);

double pq_diff_timers(struct timeval *start, struct timeval *end);
struct timeval *pq_gettime(void);

#ifdef __cplusplus
}
#endif

#endif
