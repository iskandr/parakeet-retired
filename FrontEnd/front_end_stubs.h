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

typedef struct return_val {
  return_code_t return_code;
  int results_len;
  array_type *ret_types;
  int **shapes;
  int **strides;
  union {
    char *error_msg;
    void **results;
  } data;
} return_val_t;

/** Initialization function - call before using any other functions **/
void front_end_init(void);

int register_untyped_function(char *name, char **globals, int num_globals,
                              char **args, int num_args, paranode ast);

return_val_t run_function(int id, host_val *globals, int num_globals,
                          host_val *args, int num_args);

void free_return_val(return_val_t ret_val);

#endif
