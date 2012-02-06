/*
 *  type_stubs.h
 *
 *  Stubs for interacting with Parakeet's type system.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2012.
 */
#ifndef _type_STUBS_
#define _type_STUBS_

#include <caml/mlvalues.h>

typedef struct {
  value v;
} array_type_t;

typedef array_type_t* array_type;
typedef value scalar_type;
typedef value elt_type;

void type_init();

/** OCaml array and element types for use in C world. **/
extern scalar_type parakeet_bool_t;
extern elt_type parakeet_bool_elt_t;

extern scalar_type parakeet_char_t;
extern elt_type parakeet_char_elt_t;

extern scalar_type parakeet_int32_t;
extern elt_type parakeet_int32_elt_t;

extern scalar_type parakeet_int64_t;
extern elt_type parakeet_int64_elt_t;

extern scalar_type parakeet_float32_t;
extern elt_type parakeet_float32_elt_t;

extern scalar_type parakeet_float64_t;
extern elt_type parakeet_float64_elt_t;

/** Creation functions **/
array_type build_array_root(value v);
array_type mk_array_type(elt_type t, int rank);

/** Must use to free created types **/
void free_array_type(array_type t);

/** Type accessor functions **/
int get_type_rank(array_type t);
elt_type get_array_element_type(array_type t);
elt_type get_scalar_element_type(scalar_type t);
int type_is_bool(scalar_type t);
int type_is_char(scalar_type t);
int type_is_int32(scalar_type t);
int type_is_int64(scalar_type t);
int type_is_float32(scalar_type t);
int type_is_float64(scalar_type t);

#endif
