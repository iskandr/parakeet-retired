/*
 *  ocaml_interface.c
 *
 * Interface between the OCaml runtime and our internal C-based runtime.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2010.
 */

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <string.h>

#include "ocaml_functions.h"

enum host_val_tag { HOST_SCALAR, HOST_ARRAY };


/* given a PQNum value, wrap it in a HostScalar constructor */
value build_ocaml_host_scalar(value num) {
  CAMLparam1(num);
  CAMLlocal1(hostScalar);
  num = caml_alloc(1, HOST_SCALAR);
  Store_field(hostScalar, 0, num);
  CAMLreturn(hostScalar);
}

value build_ocaml_host_array (int num_bytes, value ocaml_dyn_type,
    int *shape, int shape_len, char *flattened) {
  CAMLparam1(ocaml_dyn_type);
  CAMLlocal5(ocaml_shape, ocaml_host_ptr, ocaml_num_bytes,
      ocaml_tuple, ocaml_host_val);


  ocaml_host_val = caml_alloc(1, HOST_ARRAY);

  // tuple is the only data element of a host array
  ocaml_tuple = caml_alloc_tuple(4);

  // Non-scalar - make a shape array
  ocaml_shape = caml_alloc(shape_len, 0);
  int i;
  for(i=0; i< shape_len; i++) {
      Store_field(ocaml_shape, i, Val_int(shape[i]));
  }
  ocaml_host_ptr = caml_copy_int64((int64_t)flattened);
  ocaml_num_bytes = Val_int(num_bytes);

  // Fill the host_array record
  Store_field(ocaml_tuple, 0, ocaml_host_ptr);
  Store_field(ocaml_tuple, 1, ocaml_dyn_type);
  Store_field(ocaml_tuple, 2, ocaml_shape);
  Store_field(ocaml_tuple, 3, ocaml_num_bytes);

  // put the host_array record into the host_val
  Store_field(ocaml_host_val, 0, ocaml_tuple);
  CAMLreturn(ocaml_host_val);
}




enum pqnum_tag {
    CHAR, BOOL, UINT16, INT16, UINT32, INT32, UINT64, INT64, FLOAT32, FLOAT64
};



value build_pqnum_int32(int32_t i) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_alloc(1, INT32);
  Store_field(num, 0, copy_int32(i));
  CAMLreturn(num);
}

// wrap PQNum.Int32 with a HostScalar tag
value build_host_int32(int32_t i) {
  return build_ocaml_host_scalar(build_pqnum_int32(i));
}

value build_pqnum_int64(int64_t i) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_alloc(1, INT64);
  Store_field(num, 0, copy_int64(i));
  CAMLreturn(num);
}

// wrap PQNum.Int64 with a HostScalar tag
value build_host_int64(int64_t i) {
  return build_ocaml_host_scalar(build_pqnum_int64(i));
}

value build_pqnum_float32(float f) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_alloc(1, FLOAT32);
  Store_double_field(num, 0, f);
  CAMLreturn(num);
}

// wrap PQNum.Float32 with a HostScalar tag
value build_host_float32(float f) {
  return build_ocaml_host_scalar(build_pqnum_float32(f));
}


value build_pqnum_float64(double d) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_alloc(1, FLOAT64);
  Store_double_field(num, 0, d);
  CAMLreturn(num);
}

// wrap PQNum.Float32 with a HostScalar tag
value build_host_float64(double d) {
  return build_ocaml_host_scalar(build_pqnum_float64(d));
}


