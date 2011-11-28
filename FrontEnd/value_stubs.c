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

#include "value_stubs.h"

/** Private members **/

value *value_callback_of_bool = NULL;
value *value_callback_of_char = NULL;
value *value_callback_of_int32 = NULL;
value *value_callback_of_int64 = NULL;
value *value_callback_of_float32 = NULL;
value *value_callback_of_float64 = NULL;

value *value_callback_to_bool = NULL;
value *value_callback_to_char = NULL;
value *value_callback_to_int32 = NULL;
value *value_callback_to_int64 = NULL;
value *value_callback_to_float64 = NULL;

value *value_callback_mk_array = NULL;
value *value_callback_is_scalar = NULL;
value *value_callback_type_of = NULL;
value *value_callback_get_shape = NULL;
value *value_callback_get_strides = NULL;
value *value_callback_extract = NULL;

value *host_memspace_callback_mk_host_ptr = NULL;
value *ptr_callback_addr = NULL;
value *ptr_callback_size = NULL;

/** Public interface **/

int value_inited = 0;
void value_init() {
  if (value_inited == 0) {
    value_inited = 1;
    value_callback_of_bool = caml_named_value("value_of_bool");
    value_callback_of_char = caml_named_value("value_of_char");
    value_callback_of_int32 = caml_named_value("value_of_int32");
    value_callback_of_int64 = caml_named_value("value_of_int64");
    value_callback_of_float32 = caml_named_value("value_of_float32");
    value_callback_of_float64 = caml_named_value("value_of_float64");

    value_callback_to_bool = caml_named_value("value_to_bool");
    value_callback_to_char = caml_named_value("value_to_char");
    value_callback_to_int32 = caml_named_value("value_to_int32");
    value_callback_to_int64 = caml_named_value("value_to_int64");
    value_callback_to_float64 = caml_named_value("value_to_float64");

    value_callback_mk_array = caml_named_value("value_array");
    value_callback_is_scalar = caml_named_value("value_is_scalar");
    value_callback_type_of = caml_named_value("value_type_of");
    value_callback_get_shape = caml_named_value("value_get_shape");
    value_callback_get_strides = caml_named_value("value_get_strides");
    value_callback_extract = caml_named_value("value_extract");

    host_memspace_callback_mk_host_ptr = caml_named_value("mk_host_ptr");
    ptr_callback_addr = caml_named_value("ptr_addr");
    ptr_callback_size = caml_named_value("ptr_size");
  }
}

int value_is_scalar(host_val v) {
  CAMLparam1(v);
  CAMLreturnT(int, Int_val(caml_callback(*value_callback_is_scalar, v)));
}

value value_type_of(host_val v) {
  CAMLparam1(v);
  CAMLreturn(caml_callback(*value_callback_type_of, v));
}

value value_get_shape(host_val v) {
  CAMLparam1(v);
  CAMLreturn(caml_callback(*value_callback_get_shape, v));
}

value value_get_strides(host_val v) {
  CAMLparam1(v);
  CAMLreturn(caml_callback(*value_callback_get_strides, v));
}

host_val mk_bool(int b) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_callback(*value_callback_of_bool, Val_int(b));
  caml_register_global_root(&num);
  CAMLreturnT(host_val, num);
}

// TODO: Unclear whether this works
host_val mk_char(char val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_callback(*value_callback_of_char, Val_int(val));
  caml_register_global_root(&num);
  CAMLreturnT(host_val, num);
}

host_val mk_int32(int32_t val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_callback(*value_callback_of_int32, caml_copy_int32(val));
  caml_register_global_root(&num);
  CAMLreturnT(host_val, num);
}

host_val mk_int(int64_t val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_callback(*value_callback_of_int64, copy_int64(val));
  caml_register_global_root(&num);
  CAMLreturnT(host_val, num);
}

host_val mk_float32(float val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_callback(*value_callback_of_float32, copy_double(val));
  caml_register_global_root(&num);
  CAMLreturnT(host_val, num);
}

host_val mk_float64(double val) {
  CAMLparam0();
  CAMLlocal1(num);
  num = caml_callback(*value_callback_of_float64, copy_double(val));
  caml_register_global_root(&num);
  CAMLreturnT(host_val, num);
}

host_val mk_host_array(char *data, elt_type t, int *shape, int shape_len,
                       int *strides, int strides_len, int num_bytes) {
  CAMLparam0();
  CAMLlocal4(ocaml_host_array, ocaml_host_ptr, ocaml_shape, ocaml_strides);
  CAMLlocal1(ocaml_mk_array_args);

  // Create and register a host ptr with the Parakeet Data Manager
  ocaml_host_ptr = caml_callback2(*host_memspace_callback_mk_host_ptr,
                                  caml_copy_int64((int64_t)data),
                                  Val_int(num_bytes));

  // Create an OCaml Shape array
  ocaml_shape = caml_alloc(shape_len, 0);
  int i;
  for(i = 0; i < shape_len; i++) {
    Store_field(ocaml_shape, i, Val_int(shape[i]));
  }
  
  // Create an OCaml strides array
  ocaml_strides = caml_alloc(strides_len, 0);
  for (i = 0; i < strides_len; ++i) {
    Store_field(ocaml_strides, i, Val_int(strides[i]));
  }

  // Create the final host array Value and return it
  value mk_array_args[4];
  mk_array_args[0] = ocaml_host_ptr;
  mk_array_args[1] = (value)t;
  mk_array_args[2] = ocaml_shape;
  mk_array_args[3] = ocaml_strides;
  caml_register_global_root(&ocaml_host_array);
  ocaml_host_array = caml_callbackN(*value_callback_mk_array, 4,
                                    mk_array_args);
  CAMLreturnT(host_val, (host_val)ocaml_host_array);
}

void free_host_val(host_val val) {
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = (value)val;
  caml_remove_global_root(&ocaml_host_val);

  CAMLreturn0;
}

void free_host_val_data(host_val_data_t data) {
  // TODO: do we need to free the data field? Does Parakeet lose ownership of
  //       it when it passes it back to the Front End language?
  //       For now, I'm not freeing it, but perhaps I should be.

  if (data.shape) free(data.shape);
}

int host_val_is_scalar(host_val val) {
  CAMLparam0();
  CAMLreturnT(int, Int_val(caml_callback(*value_callback_is_scalar, val)));
}

/** Scalar Accessors **/

int get_bool(host_val val) {
  CAMLparam0();
  CAMLreturnT(int, Int_val(caml_callback(*value_callback_to_bool, val)));
}

/*
 * TODO: Still don't know exactly what to do about chars.
char get_char(host_val val);
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = (value)val;

  CAMLreturnT();
}
*/

int32_t get_int32(host_val val) {
  CAMLparam0();
  CAMLreturnT(int32_t, Int32_val(caml_callback(*value_callback_to_int32, val)));
}

int64_t  get_int64(host_val val) {
  CAMLparam0();
  CAMLreturnT(int64_t, Int64_val(caml_callback(*value_callback_to_int64, val)));
}

/*
 * TODO: Why don't we support this yet?
float get_float32(host_val val) {
  CAMLparam0();
  CAMLlocal1(ocaml_host_val);

  ocaml_host_val = (value)val;

  CAMLreturnT(float, (float)Double_val(Field(Field(ocaml_host_val, 0), 0)));
}
*/

double get_float64(host_val val) {
  CAMLparam0();
  CAMLreturnT(double,
              Double_val(caml_callback(*value_callback_to_float64, val)));
}

/** Non-Scalar Accessors **/
array_type get_host_val_array_type(host_val val) {
  CAMLparam0();
  CAMLlocal2(ocaml_host_val, ocaml_dyn_type);

  ocaml_host_val = (value)val;

  caml_register_global_root(&ocaml_dyn_type);
  ocaml_dyn_type = Field(Field(ocaml_host_val, 0), 1);
  CAMLreturnT(array_type, (array_type)ocaml_dyn_type);
}

host_val_data_t get_host_val_array_data(host_val val) {
  CAMLparam0();
  CAMLlocal3(ocaml_host_val, ocaml_array, ocaml_shape);

  ocaml_host_val = (value)val;
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

void* get_array_data(host_val array) {
  CAMLparam0();
  CAMLlocal1(ocaml_data);
  
  ocaml_data = Field(caml_callback(*value_callback_extract, array), 0);
  CAMLreturnT(void*,
              (void*)Int64_val(caml_callback(*ptr_callback_addr, ocaml_data)));
}
