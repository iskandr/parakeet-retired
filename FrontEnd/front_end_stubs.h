/*
 *  front_end_stubs.h
 *
 *  Front end functions for registering and running functions with the Parakeet
 *  runtime.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2011.
 */

#ifndef _FRONT_END_STUBS_H_
#define _FRONT_END_STUBS_H_

#include "ast_stubs.h"
#include "type_stubs.h"
#include "value_stubs.h"

typedef enum {
  RET_SUCCESS = 0,
  RET_FAIL,
  RET_PASS
} return_code_t;

typedef struct {
  elt_type ret_type;
  union {
    int       boolean;
    int32_t   int32;
    int64_t   int64;
    float     float32;
    double    float64;
  } ret_scalar_value;
} scalar_ret_t;

typedef struct {
  array_type ret_type;
  void *data;
  int  *shape;
  int  shape_len;
  int  *strides;
  int  strides_len;
} array_ret_t;

typedef struct {
  union {
    array_ret_t array;
    scalar_ret_t scalar;
  } data;
  int is_scalar;
} ret_t;

typedef struct return_val {
  return_code_t return_code;
  int results_len;
  char *error_msg;
  ret_t *results;
} return_val_t;

/** Initialization function - call before using any other functions **/
void front_end_init(void);

/** Execution interface **/
int register_untyped_function(char *name, char **globals, int num_globals,
                              char **args, int num_args, paranode ast);

return_val_t run_function(int id, host_val *globals, int num_globals,
                          host_val *args, int num_args);

void free_return_val(return_val_t ret_val);

/** Parakeet parameter configuration **/
void set_vectorize(int val);

#endif
