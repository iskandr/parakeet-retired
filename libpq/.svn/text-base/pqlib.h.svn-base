/*
 * Parallel Q Library
 *
 * (c) 2009 Eric Hielscher
 *
 * CUDA implementation of the identity function on arrays.
 */
#ifndef __PQLIB_PQLIB_H__
#define __PQLIB_PQLIB_H__

#include <cuda.h>
#include <time.h>

#ifdef __cplusplus
extern "C" {
#endif

void pq_init(void);

CUmodule *pq_compile_module(char *PTXcode);
void pq_destroy_module(CUmodule *cuModule);

void pq_launch_ptx(CUmodule *cuModule, char *PTXfunc,
                   void **args, int numArgs, int *aligns, int *sizes,
                   int threadsx, int threadsy, int threadsz,
                   int gridwidth, int gridheight);

void pq_memcpy_to_constant(const void *data, unsigned int bytes,
                           CUmodule *cuModule, const char *symbol);

// Timing functions
double pq_time_mem_transfer(int num_bytes);

// Frees the timespecs
double pq_diff_timers(struct timespec *start, struct timespec *end);
// Allocs the timespec
struct timespec *pq_gettime(void);

#ifdef __cplusplus
}
#endif

#endif // __PQLIB_PQLIB_H__
