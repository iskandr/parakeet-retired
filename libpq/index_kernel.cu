/*
 * Parallel Q Library
 *
 * (c) 2009-2010 Eric Hielscher
 *
 * Each-left each-right implementations.
 */

#include <cuda.h>
#include <stdio.h>

#define THREADS_PER_BLOCK 256

#ifdef __cplusplus
extern "C" {
#endif

texture<int, 1, cudaReadModeElementType> indexIdxsTex;

texture<float, 1, cudaReadModeElementType> indexFloatVecsTex;
__global__
void index_int_kernel(int num_vecs, int vec_len, int num_idxs, int *output) {
  /* Our algorithm we'll try here: assign one thread per output element.
   * This should get perfect output coalescing, and the inputs are both in
   * textures so we should get fine input locality on the input reads.
   * TODO: look up how the texture caches actually work so that we are really
   *       getting efficiency in the vec reads.
   */
  int id = blockIdx.y * 16384 * THREADS_PER_BLOCK +
           blockIdx.x * THREADS_PER_BLOCK + threadIdx.x;
  if (id < num_idxs * vec_len) {
    int idx = id / vec_len;
    int offset = id - (idx * vec_len);
    int vec = tex1Dfetch(indexIdxsTex, idx);
    output[id] = tex1Dfetch(indexFloatVecsTex, vec*vec_len + offset);
  }
}

texture<int, 1, cudaReadModeElementType> indexIntVecsTex;
__global__
void index_float_kernel(int num_vecs, int vec_len, int num_idxs,
                        float *output) {
  /* Our algorithm we'll try here: assign one thread per output element.
   * This should get perfect output coalescing, and the inputs are both in
   * textures so we should get fine input locality on the input reads.
   * TODO: look up how the texture caches actually work so that we are really
   *       getting efficiency in the vec reads.
   */
  int id = blockIdx.y * 16384 * THREADS_PER_BLOCK +
           blockIdx.x * THREADS_PER_BLOCK + threadIdx.x;
  if (id < num_idxs * vec_len) {
    int idx = id / vec_len;
    int offset = id - (idx * vec_len);
    int vec = tex1Dfetch(indexIdxsTex, idx);
    output[id] = tex1Dfetch(indexIntVecsTex, vec*vec_len + offset);
  }
}

#ifdef __cplusplus
}
#endif
