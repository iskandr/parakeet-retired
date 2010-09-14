/*
 * Parallel Q Library
 *
 * (c) 2009-2010 Eric Hielscher
 *
 * Main libpq interface functions.
 */

#include <cuda.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "pqlib.h"

#define ALIGN_UP(offset, alignment) \
  (offset) = ((offset) + (alignment) - 1) & ~((alignment) - 1)

#ifdef __cplusplus
extern "C" {
#endif

void pq_init() {
  if (cuInit(0) != CUDA_SUCCESS) {
    exit (0);
  }
  printf("pq inited\n");
}

void pq_destroy_module(CUmodule *cuModule) {
  free(cuModule);
}

void pq_memcpy_to_constant(const void *data, unsigned int bytes,
                           CUmodule *cuModule, const char *symbol) {
  CUdeviceptr devPtr;
  unsigned int dummybytes;
  cuModuleGetGlobal(&devPtr, &dummybytes, *cuModule, symbol);
  cuMemcpyHtoD(devPtr, data, bytes);
}

// Device Query Functions


// Timing functions
double pq_time_mem_transfer(int num_bytes) {
  timespec *start, *end;

  void *host_data = malloc(num_bytes);
  memset(host_data, 0, num_bytes);

  CUdeviceptr devptr;
  cuMemAlloc(&devptr, num_bytes);

  start = pq_gettime();
  cuMemcpyHtoD(devptr, host_data, num_bytes);
  end = pq_gettime();

  free(host_data);
  cuMemFree(devptr);

  return pq_diff_timers(start, end);
}

double pq_diff_timers(struct timespec *start, struct timespec *end) {
  double ret;

  if (end->tv_nsec < start->tv_nsec) {
    int nsec = (start->tv_nsec - end->tv_nsec) / 1000000000 + 1;
    start->tv_nsec -= 1000000000 * nsec;
    start->tv_sec += nsec;
  }
  if (end->tv_nsec - start->tv_nsec > 1000000000) {
    int nsec = (end->tv_nsec - start->tv_nsec) / 1000000000;
    start->tv_nsec += 1000000000 * nsec;
    start->tv_sec -= nsec;
  }

  ret = (end->tv_sec - start->tv_sec) +
        (end->tv_nsec - start->tv_nsec) / 1000000000.0;

  free(start);
  free(end);

  return ret;
}

struct timespec *pq_gettime(void) {
  struct timespec *ret = (struct timespec*)(malloc(sizeof(struct timespec)));
  clock_gettime(CLOCK_REALTIME, ret);
  return ret;
}

#ifdef __cplusplus
}
#endif

