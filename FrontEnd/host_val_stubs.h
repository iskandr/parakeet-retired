/*
 * host_val_stubs.h
 *
 * Interface for front ends to create Parakeet HostVals.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2011.
 */
#ifndef _HOST_VAL_STUBS_H_
#define _HOST_VAL_STUBS_H_

#include <stdint.h>

#include "dyn_type_stubs.h"

typedef void *host_val;

typedef struct host_val_data {
  void *data;
  int  *shape;
  int  shape_len;
  int  num_bytes;
} host_val_data_t;

/** Scalar creation functions **/
host_val mk_bool(int val);
host_val mk_char(char val);
//host_val mk_uint16(uint16_t val);
//host_val mk_int16(int16_t val);
host_val mk_uint32(uint32_t val);
host_val mk_int32(int32_t val);
host_val mk_uint64(uint64_t val);
host_val mk_int64(int64_t val);
host_val mk_float32(float val);
host_val mk_float64(double val);

host_val mk_inf(dyn_type t);
host_val mk_neginf(dyn_type t);

/** Array creation function **/
host_val mk_host_array(char *data, dyn_type t, int *shape, int shape_len,
                       int num_bytes);

/** IMPORTANT: Must call to free OCaml host_val **/
void free_host_val(host_val val);
void free_host_val_data(host_val_data_t data);
int is_scalar(host_val val);

/** Scalar accessor functions **/
int      get_bool(host_val val);
//char     get_char(host_val val); TODO: implement this
//uint16_t get_uint16(host_val val);
//int16_t  get_int16(host_val val);
uint32_t get_uint32(host_val val);
int32_t  get_int32(host_val val);
uint64_t get_uint64(host_val val);
int64_t  get_int64(host_val val);
float    get_float32(host_val val);
double   get_float64(host_val val);

/** Non-scalar accessor functions **/
// Gives caller ownership of dyn_type's memory (must call free later)
// Undefined behavior when called on non-array type
dyn_type get_host_val_array_type(host_val val);

host_val_data_t get_host_val_array_data(host_val val);

#endif

