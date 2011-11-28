/*
 * host_val_stubs.h
 *
 * Interface for front ends to create Parakeet HostVals.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2011.
 */
#ifndef _HOST_VAL_STUBS_H_
#define _HOST_VAL_STUBS_H_

#include <caml/mlvalues.h>
#include <stdint.h>

#include "type_stubs.h"

typedef value host_val;

void init();

typedef struct host_val_data {
  void *data;
  int  num_bytes;
  int  *shape;
  int  shape_len;
  int  *stride;
  int  stride_len;
} host_val_data_t;

/** Scalar creation functions **/
host_val mk_bool(int val);
host_val mk_char(char val);
host_val mk_int32(int32_t val);
host_val mk_int64(int64_t val);
host_val mk_float32(float val);
host_val mk_float64(double val);

/** Array creation function **/
host_val mk_host_array(char *data, elt_type t,
                       int *shape, int shape_len,
                       int *strides, int strides_len,
                       int num_bytes);

/** IMPORTANT: Must call to free on OCaml host_vals created **/
void free_host_val(host_val val);
void free_host_val_data(host_val_data_t data);

int host_val_is_scalar(host_val val);

/** Scalar accessor functions **/
int      get_bool(host_val val);
int32_t  get_int32(host_val val);
int64_t  get_int64(host_val val);
//float    get_float32(host_val val);
double   get_float64(host_val val);

/** Non-scalar accessor functions **/
// Gives caller ownership of dyn_type's memory (must call free later)
// Undefined behavior when called on non-array type
array_type get_host_val_array_type(host_val val);
void* get_array_data(host_val array);

//host_val_data_t get_host_val_array_data(host_val val);

#endif
