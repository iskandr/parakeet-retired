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
#include "host_val_stubs.h"

#define RET_MSG_MAX_LEN 256

typedef enum {
  RET_SUCCESS = 0,
  RET_PASS,
  RET_FAIL
} return_code_t;

typedef struct return_val {
  return_code_t return_code;
  dyn_type ret_type;
  int num_results;
  union {
    char error_msg[RET_MSG_MAX_LEN];
    void **results;
  } data;
} return_val_t;

/** Initialization function - call before using any other functions **/
void front_end_init(void);

int32_t register_untyped_function(char *name, char **globals, int num_globals,
                                  char **args, int num_args, paranode ast);

return_val_t run_function(int32_t id, host_val *globals, int num_globals,
                          host_val *args, int num_args);

#endif
