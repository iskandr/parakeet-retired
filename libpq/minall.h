#include <cuda.h>

#ifdef __cplusplus
extern "C" {
#endif

void minall(float *input, int *output,
                   int input_x, int input_y, int include_mem_in_time);

#ifdef __cplusplus
}
#endif

