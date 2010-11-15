#ifdef __cplusplus
extern "C" {
#endif

void launch_where(int test_val, int *input, int input_len, int *output);
void launch_where_dev(int test_val, int *input, int input_len, int *output,
                      int memtime);

#ifdef __cplusplus
}
#endif
