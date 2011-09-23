#ifndef _DYN_TYPE_STUBS_
#define _DYN_TYPE_STUBS_

#include <caml/mlvalues.h>
#include "variants.h"


typedef value dyn_type;
/*
typedef struct dt {
  int scalar;
  union {
    dyn_type_no_data_t   no_data_tag;
    dyn_type_with_data_t data_tag;
  } tag;
} dt_t;
*/
void dyn_type_init();

/** Creation functions **/
//dyn_type mk_scalar(dyn_type_no_data_t t);
dyn_type mk_vec(dyn_type t);

/** Must use to free created dyn_types **/
void free_dyn_type(dyn_type t);

/** Type accessor functions **/
int get_dyn_type_rank(dyn_type t);
dyn_type get_dyn_type_element_type(dyn_type t);
int dyn_type_is_scalar(dyn_type t);
dyn_type get_type_tag(dyn_type t);

// Undefined behavior if called on a scalar type
// Gives ownership over the subtype's memory to the caller
dyn_type get_subtype(dyn_type t);

dyn_type parakeet_bool_t;
dyn_type parakeet_char_t;
dyn_type parakeet_int32_t;
dyn_type parakeet_int64_t;
dyn_type parakeet_float32_t;
dyn_type parakeet_float64_t;
#endif
