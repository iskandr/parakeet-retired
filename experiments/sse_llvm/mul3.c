void mul_3(int *input, int *output) {
  int i;
  for (i = 0; i < 32000; ++i) {
    output[i] = input[i] * 3;
  }
}
