/*
 * Parallel Q Library
 *
 * (c) 2010 Eric Hielscher, Alex Rubinsteyn
 *
 * OCaml interface for Thrust library
 */

#include <cuda.h>

#include "thrust/device_vector.h"
#include "thrust/scan.h"

extern "C" {

void scan_bool(bool *input, int input_len, bool *output) {
  thrust::device_ptr<bool> input_ptr(input);
  thrust::device_ptr<bool> output_ptr(output);
  thrust::inclusive_scan(input_ptr, input_ptr + input_len, output_ptr);
}

void scan_bool_to_int(bool *input, int input_len, int *output) {
  thrust::device_ptr<bool> input_ptr(input);
  thrust::device_ptr<int> output_ptr(output);
  thrust::inclusive_scan(input_ptr, input_ptr + input_len, output_ptr);
}
  
void scan_int(int *input, int input_len, int *output) {
  thrust::device_ptr<int> input_ptr(input);
  thrust::device_ptr<int> output_ptr(output);
  thrust::inclusive_scan(input_ptr, input_ptr + input_len, output_ptr);
}

void scan_float(float *input, int input_len, float *output) {
  thrust::device_ptr<float> input_ptr(input);
  thrust::device_ptr<float> output_ptr(output);
  thrust::inclusive_scan(input_ptr, input_ptr + input_len, output_ptr);
}

}
