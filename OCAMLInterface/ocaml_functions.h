/*
 *  ocaml_functions.h
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

void parakeet_init(void);

value build_ocaml_host_scalar(value num);
value build_ocaml_host_array(int num_bytes, value ocaml_dyn_type,
                             int *shape, int shape_len, char *flattened);

/* wrap numbers in PQNum variant */
value build_pqnum_int64(int64_t);
value build_pqnum_int32(int32_t);
value build_pqnum_float32(float);
value build_pqnum_float64(double);

/* wrap numbers in HostVal constructor HostScalar */
value build_host_int32(int32_t);
value build_host_int64(int64_t);
value build_host_float32(float);
value build_host_float64(double);

/* unwrap PQNum variants */
int32_t get_pqnum_int32(value);
int64_t get_pqnum_int64(value);
float get_pqnum_float32(value);
double get_pqnum_float64(value);
