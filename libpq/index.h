#include <cuda.h>

#ifdef __cplusplus
extern "C" {
#endif

void launch_index(int *vecs, int num_vecs, int *idxs, int num_idxs, int vec_len,
                  int *output);
void launch_index_dev(int *vecs, int num_vecs, int *idxs, int num_idxs,
                      int vec_len, int *output, int memtime);

#ifdef __cplusplus
}
#endif
