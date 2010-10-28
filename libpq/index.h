#include <cuda.h>

#ifdef __cplusplus
extern "C" {
#endif

void launch_index(int *vecs, int num_vecs, int *idxs, int num_idxs, int vec_len,
                  int *output);

#ifdef __cplusplus
}
#endif
