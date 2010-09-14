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

value build_ocaml_hostval(int num_bytes, value ocaml_dyn_type,
                          int *shape, int shape_len, char *flattened) {
  CAMLparam1(ocaml_dyn_type);
  CAMLlocal4(ocaml_shape, ocaml_host_ptr, ocaml_num_bytes, ocaml_var);

  printf("in build hostval\n");

  // Allocate the outer host_val record
  ocaml_var = caml_alloc_tuple(4);

  if (shape_len == 0) {
    // Scalar - empty shape vector
    // TODO: Cacheing the callback pointer is more efficient
    //ocaml_shape = caml_callback(*caml_named_value("shape_create"), Val_int(0));
    ocaml_shape = alloc_bigarray(BIGARRAY_INT32 | BIGARRAY_C_LAYOUT,
                                 0, NULL, NULL);
    printf("built empty bigarray\n");
  } else {
    // Non-scalar - make bigarray
    intnat dims[1];
    dims[0] = shape_len;
    ocaml_shape = alloc_bigarray(BIGARRAY_INT32 | BIGARRAY_C_LAYOUT,
                                 1, shape, dims);
  }
  printf("created shape\n");

  ocaml_host_ptr = caml_copy_int64((int64_t)flattened);  
  ocaml_num_bytes = Val_int(num_bytes);

  // Fill the host_val record
  Store_field(ocaml_var, 0, ocaml_host_ptr);
  Store_field(ocaml_var, 1, ocaml_num_bytes);
  Store_field(ocaml_var, 2, ocaml_dyn_type);
  Store_field(ocaml_var, 3, ocaml_shape);

  CAMLreturn(ocaml_var);
}


