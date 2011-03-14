/*
 * host_val_stubs.c
 *
 * Interface for front ends to create Parakeet HostVals.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2011.
 */

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "host_val_stubs.h"

/** Private members **/
static host_val build_ocaml_host_scalar(value num);

/** Public interface **/

host_val mk_bool(int val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_alloc(1, PQNUM_BOOL);
  Store_field(num, 0, copy_int32(val));
  CAMLreturnT(host_val, build_ocaml_host_scalar(num));
}

// TODO: Unclear whether this works
host_val mk_char(char val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_alloc(1, PQNUM_CHAR);
  int i = (int)val;
  Store_field(num, 0, copy_int32(i));
  CAMLreturnT(host_val, build_ocaml_host_scalar(num));
}

host_val mk_uint16(unsigned val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_alloc(1, PQNUM_UINT16);
  Store_field(num, 0, copy_int32(val));
  CAMLreturnT(host_val, build_ocaml_host_scalar(num));
}

host_val mk_int16(int val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_alloc(1, PQNUM_INT16);
  Store_field(num, 0, copy_int32(val));
  CAMLreturnT(host_val, build_ocaml_host_scalar(num));
}

host_val mk_uint32(uint32_t val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_alloc(1, PQNUM_UINT32);
  Store_field(num, 0, copy_int32(val));
  CAMLreturnT(host_val, build_ocaml_host_scalar(num));
}

host_val mk_int32(int32_t val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_alloc(1, PQNUM_INT32);
  Store_field(num, 0, copy_int32(val));
  CAMLreturnT(host_val, build_ocaml_host_scalar(num));
}

host_val mk_uint64(uint64_t val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_alloc(1, PQNUM_UINT64);
  Store_field(num, 0, copy_int64(val));
  CAMLreturnT(host_val, build_ocaml_host_scalar(num));
}

host_val mk_int(int64_t val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_alloc(1, PQNUM_INT64);
  Store_field(num, 0, copy_int64(val));
  CAMLreturnT(host_val, build_ocaml_host_scalar(num));
}

host_val mk_float32(float val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_alloc(1, PQNUM_FLOAT32);
  Store_field(num, 0, copy_double(val));
  CAMLreturnT(host_val, build_ocaml_host_scalar(num));
}

host_val mk_float64(double val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_alloc(1, PQNUM_FLOAT64);
  Store_field(num, 0, copy_double(val));
  CAMLreturnT(host_val, build_ocaml_host_scalar(num));
}

host_val mk_inf(dyn_type t) {
  CAMLparam0();
  CAMLlocal2(num, ocaml_t);
  num = caml_alloc(1, PQNUM_INF);
  ocaml_t = *(value*)t;
  Store_field(num, 0, ocaml_t);
  CAMLreturnT(host_val, build_ocaml_host_scalar(num));
}

host_val mk_neginf(dyn_type t) {
  CAMLparam0();
  CAMLlocal2(num, ocaml_t);
  num = caml_alloc(1, PQNUM_NEGINF);
  ocaml_t = *(value*)t;
  Store_field(num, 0, ocaml_t);
  CAMLreturnT(host_val, build_ocaml_host_scalar(num));
}

host_val mk_host_array(char *data, dyn_type t, int *shape, int shape_len,
                       int num_bytes) {
  CAMLparam0();
  CAMLlocal5(ocaml_shape, ocaml_host_ptr, ocaml_num_bytes,
      ocaml_tuple, ocaml_host_val);
  CAMLlocal1(ocaml_dyn_type);

  ocaml_dyn_type = *(value*)t;

  ocaml_host_val = caml_alloc(1, HostArray);

  // tuple is the only data element of a host array
  ocaml_tuple = caml_alloc_tuple(4);

  // Non-scalar - make a shape array
  ocaml_shape = caml_alloc(shape_len, 0);
  int i;
  for(i = 0; i < shape_len; i++) {
      Store_field(ocaml_shape, i, Val_int(shape[i]));
  }
  ocaml_host_ptr = caml_copy_int64((int64_t)data);
  ocaml_num_bytes = Val_int(num_bytes);

  // Fill the host_array record
  Store_field(ocaml_tuple, 0, ocaml_host_ptr);
  Store_field(ocaml_tuple, 1, ocaml_dyn_type);
  Store_field(ocaml_tuple, 2, ocaml_shape);
  Store_field(ocaml_tuple, 3, ocaml_num_bytes);

  // put the host_array record into the host_val
  caml_register_global_root(&ocaml_host_val);
  Store_field(ocaml_host_val, 0, ocaml_tuple);
  CAMLreturnT(host_val, (host_val)&ocaml_host_val);
}

void free_host_val(host_val val) {
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = *(value*)val;
  caml_remove_global_root(&ocaml_host_val);

  CAMLreturn0;
}

void free_host_val_data(host_val_data_t data) {
  // TODO: do we need to free the data field? Does Parakeet lose ownership of
  //       it when it passes it back to the Front End language?
  //       For now, I'm not freeing it, but perhaps I should be.

  if (data.shape) free(data.shape);
}

int is_scalar(host_val val) {
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = *(value*)val;
  int ret = Tag_val(ocaml_host_val) == HostScalar;

  CAMLreturnT(int, ret);
}

/** Scalar Accessors **/

int get_bool(host_val val) {
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = *(value*)val;

  CAMLreturnT(int, (int)Bool_val(Field(Field(ocaml_host_val, 0), 0)));
}

/*
char get_char(host_val val);
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = *(value*)val;

  CAMLreturnT();
}
*/

/*
uint16_t get_uint16(host_val val);
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = *(value*)val;

  CAMLreturnT(uint);
}

int16_t  get_int16(host_val val);
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = *(value*)val;

  CAMLreturnT();
}
*/

uint32_t get_uint32(host_val val) {
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = *(value*)val;

  CAMLreturnT(uint32_t, Int32_val(Field(Field(ocaml_host_val, 0), 0)));
}

int32_t get_int32(host_val val) {
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = *(value*)val;

  CAMLreturnT(int32_t, Int32_val(Field(Field(ocaml_host_val, 0), 0)));
}

uint64_t get_uint64(host_val val) {
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = *(value*)val;

  CAMLreturnT(uint64_t, Int64_val(Field(Field(ocaml_host_val, 0), 0)));
}

int64_t  get_int64(host_val val) {
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = *(value*)val;

  CAMLreturnT(int64_t, Int64_val(Field(Field(ocaml_host_val, 0), 0)));
}

float get_float32(host_val val) {
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = *(value*)val;

  CAMLreturnT(float, (float)Double_val(Field(Field(ocaml_host_val, 0), 0)));
}

double get_float64(host_val val) {
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = *(value*)val;

  CAMLreturnT(double, Double_val(Field(Field(ocaml_host_val, 0), 0)));
}

/** Non-Scalar Accessors **/
dyn_type get_host_val_array_type(host_val val) {
  CAMLparam0();
  CAMLlocal2(ocaml_host_val, ocaml_dyn_type);

  ocaml_host_val = *(value*)val;

  caml_register_global_root(&ocaml_dyn_type);
  ocaml_dyn_type = Field(Field(ocaml_host_val, 0), 1);
  CAMLreturnT(dyn_type, (dyn_type)&ocaml_dyn_type);
}

host_val_data_t get_host_val_array_data(host_val val) {
  CAMLparam0();
  CAMLlocal3(ocaml_host_val, ocaml_array, ocaml_shape);

  ocaml_host_val = *(value*)val;
  ocaml_array    = Field(ocaml_host_val, 0);
  ocaml_shape    = Field(ocaml_array, 2);

  host_val_data_t ret;
  ret.data      = (void*)Int64_val(Field(ocaml_array, 0));
  ret.shape_len = Wosize_val(ocaml_shape);
  ret.shape = (int*)malloc(sizeof(int) * ret.shape_len);
  int i;
  for (i = 0; i < ret.shape_len; ++i) {
    ret.shape[i] = Int_val(Field(ocaml_shape, i));
  }
  
  ret.num_bytes = Int_val(Field(ocaml_array, 3));
  
  CAMLreturnT(host_val_data_t, ret);
}

/** Private members **/

/* given a PQNum value, wrap it in a HostScalar constructor */
host_val build_ocaml_host_scalar(value num) {
  CAMLparam1(num);
  CAMLlocal1(hostScalar);
  hostScalar = caml_alloc(1, HostScalar);
  caml_register_global_root(&hostScalar);
  Store_field(hostScalar, 0, num);
  CAMLreturnT(host_val, (host_val)&hostScalar);
}

