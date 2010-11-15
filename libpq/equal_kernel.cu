/*
 * Parallel Q Library
 *
 * (c) 2010 Eric Hielscher, Alex Rubinsteyn
 *
 * OCaml interface for Thrust library
 */

#include <cuda.h>

#include "thrust/device_vector.h"
#include "thrust/equal.h"

extern "C" {

bool equal_int(int *left, int *right, int input_len) {
  thrust::device_ptr<int> left_ptr((int*)left);
  thrust::device_ptr<int> right_ptr((int*)right);
  return thrust::equal(left_ptr, left_ptr + input_len, right_ptr);
}

}
