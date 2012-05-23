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

/** Scalar creation functions **/
host_val mk_none_val();
host_val mk_bool(int val);
host_val mk_char(char val);
host_val mk_int32(int32_t val);
host_val mk_int64(int64_t val);
host_val mk_float32(float val);
host_val mk_float64(double val);

/** Scalar accessor functions **/
int      get_bool(host_val val);
int32_t  get_int32(host_val val);
int64_t  get_int64(host_val val);
//float    get_float32(host_val val);
double   get_float64(host_val val);

int value_is_scalar(host_val val);
array_type array_type_of(host_val v);
value value_type_of(host_val v);
value value_get_shape(host_val v); 
value value_get_strides(host_val v); 

/** Array creation function **/
/** This function doesn't copy the data; rather, the caller and Parakeet share
 *  this copy of the data.  Thus, the caller can't delete this copy of the data
 *  until after Parakeet is sure to be finished with it. **/
host_val mk_host_array(char *data, elt_type t,
                       int *shape, int shape_len,
                       int *strides, int strides_len,
                       int num_bytes);

void* get_array_data(host_val array);

/** IMPORTANT: Must call to free on OCaml host_vals created **/
void free_host_val(host_val val);

#endif
