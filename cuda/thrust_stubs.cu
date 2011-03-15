/*
 * Parallel Q Library
 *
 * (c) 2010 Eric Hielscher, Alex Rubinsteyn
 *
 * OCaml interface for Thrust library
 */

#include <cuda.h>

#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

#include "thrust/device_vector.h"
#include "thrust/scan.h"

extern "C" {

CAMLprim
value ocaml_thrust_prefix_sum_bool_to_int(value input, value input_len,
                                          value output) {
  CAMLparam3(input, input_len, output);

  thrust::device_ptr<bool> input_ptr((bool*)Int32_val(input));
  thrust::device_ptr<int> output_ptr((int*)Int32_val(output));
  thrust::inclusive_scan(input_ptr, input_ptr + Int_val(input_len), output_ptr);

  CAMLreturn(Val_unit);
}

CAMLprim
value ocaml_thrust_prefix_sum_int(value input, value input_len, value output) {
  CAMLparam3(input, input_len, output);

  thrust::device_ptr<int> input_ptr((int*)Int32_val(input));
  thrust::device_ptr<int> output_ptr((int*)Int32_val(output));
  thrust::inclusive_scan(input_ptr, input_ptr + Int_val(input_len), output_ptr);

  CAMLreturn(Val_unit);
}

CAMLprim
value ocaml_thrust_prefix_sum_float(value input, value input_len,
                                    value output) {
  CAMLparam3(input, input_len, output);
  
  thrust::device_ptr<float> input_ptr((float*)Int32_val(input));
  thrust::device_ptr<float> output_ptr((float*)Int32_val(output));
  thrust::inclusive_scan(input_ptr, input_ptr + Int_val(input_len), output_ptr);

  CAMLreturn(Val_unit);
}

}
