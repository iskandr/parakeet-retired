#ifndef _DYN_TYPE_STUBS_
#define _DYN_TYPE_STUBS_

#include "variants.h"

typedef void* dyn_type;

typedef struct dt {
  int scalar;
  union {
    dyn_type_no_data_t   no_data_tag;
    dyn_type_with_data_t data_tag;
  } tag;
} dt_t;

/** Creation functions **/
dyn_type mk_scalar(dyn_type_no_data_t t);
dyn_type mk_vec(dyn_type t);

/** Must use to free created dyn_types **/
void free_dyn_type(dyn_type t);

/** Type accessor functions **/
int is_scalar(dyn_type t);
dt_t get_type_tag(dyn_type t);

// Undefined behavior if called on a scalar type
// Gives ownership over the subtype's memory to the caller
dyn_type get_subtype(dyn_type t);

#endif

