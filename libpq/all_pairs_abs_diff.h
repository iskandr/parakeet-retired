#include <cuda.h>

#ifdef __cplusplus
extern "C" {
#endif

void all_pairs_abs_diff(int *X, int left_len,
                        int *C, int right_len,
                        int vec_len, int *output);

#ifdef __cplusplus
}
#endif
