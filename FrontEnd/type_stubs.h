#ifndef _type_STUBS_
#define _type_STUBS_

#include <caml/mlvalues.h>

typedef value array_type;
typedef value elt_type;

void type_init();

/** OCaml array and element types for use in C world. **/
extern array_type parakeet_bool_t;
extern elt_type parakeet_bool_elt_t;

extern array_type parakeet_char_t;
extern elt_type parakeet_char_elt_t;

extern array_type parakeet_int32_t;
extern elt_type parakeet_int32_elt_t;

extern array_type parakeet_int64_t;
extern elt_type parakeet_int64_elt_t;

extern array_type parakeet_float32_t;
extern elt_type parakeet_float32_elt_t;

extern array_type parakeet_float64_t;
extern elt_type parakeet_float64_elt_t;

/** Creation functions **/
array_type mk_array_type(elt_type t, int rank);

/** Must use to free created types **/
void free_array_type(array_type t);

/** Type accessor functions **/
int get_type_rank(array_type t);
elt_type get_element_type(array_type t);
int type_is_scalar(array_type t);
int type_is_bool(elt_type t);
int type_is_char(elt_type t);
int type_is_int32(elt_type t);
int type_is_int64(elt_type t);
int type_is_float32(elt_type t);
int type_is_float64(elt_type t);

// Undefined behavior if called on a scalar type
// Gives ownership over the subtype's memory to the caller
array_type get_subtype(array_type t);

#endif
