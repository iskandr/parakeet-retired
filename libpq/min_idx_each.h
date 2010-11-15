#include <cuda.h>

#ifdef __cplusplus
extern "C" {
#endif

void min_idx_each_rowmajor(int *D, int *output, int num_rows, int num_cols);

#ifdef __cplusplus
}
#endif
