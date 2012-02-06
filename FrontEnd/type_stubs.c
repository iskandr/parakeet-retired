/*
 *  type_stubs.c
 *
 *  Stubs for interacting with Parakeet's type system.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2012.
 */
#include <assert.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "type_stubs.h"

// PRIVATE
static value get_array_value(array_type t);

static int type_inited = 0;

static value *type_callback_elt_type = NULL;
static value *type_callback_rank = NULL;
static value *type_callback_mk_array = NULL;
static value *type_callback_is_scalar = NULL;

static value *type_callback_is_bool = NULL;
static value *type_callback_is_char = NULL;
static value *type_callback_is_int32 = NULL;
static value *type_callback_is_int64 = NULL;
static value *type_callback_is_float32 = NULL;
static value *type_callback_is_float64 = NULL;

// PUBLIC
scalar_type parakeet_bool_t;
elt_type parakeet_bool_elt_t;

scalar_type parakeet_char_t;
elt_type parakeet_char_elt_t;

scalar_type parakeet_int32_t;
elt_type parakeet_int32_elt_t;

scalar_type parakeet_int64_t;
elt_type parakeet_int64_elt_t;

scalar_type parakeet_float32_t;
elt_type parakeet_float32_elt_t;

scalar_type parakeet_float64_t;
elt_type parakeet_float64_elt_t;

void type_init() {
  if (type_inited == 0) {
    type_inited = 1;
    type_callback_elt_type = caml_named_value("elt_type");
    type_callback_rank = caml_named_value("type_rank");
    type_callback_mk_array = caml_named_value("mk_array");
    type_callback_is_scalar = caml_named_value("type_is_scalar");

    parakeet_bool_t = *caml_named_value("bool_t");
    parakeet_bool_elt_t = *caml_named_value("bool_elt_t");
    type_callback_is_bool = caml_named_value("type_is_bool");

    parakeet_char_t = *caml_named_value("char_t");
    parakeet_char_elt_t = *caml_named_value("char_elt_t");
    type_callback_is_char = caml_named_value("type_is_char");

    parakeet_int32_t = *caml_named_value("int32_t");
    parakeet_int32_elt_t = *caml_named_value("int32_elt_t");
    type_callback_is_int32 = caml_named_value("type_is_int32");

    parakeet_int64_t = *caml_named_value("int64_t");
    parakeet_int64_elt_t = *caml_named_value("int64_elt_t");
    type_callback_is_int64 = caml_named_value("type_is_int64");

    parakeet_float32_t = *caml_named_value("float32_t");
    parakeet_float32_elt_t = *caml_named_value("float32_elt_t");
    type_callback_is_float32 = caml_named_value("type_is_float32");

    parakeet_float64_t = *caml_named_value("float64_t");
    parakeet_float64_elt_t = *caml_named_value("float64_elt_t");
    type_callback_is_float64 = caml_named_value("type_is_float64");
  }
}

/** Public interface **/
array_type build_array_root(value v) {
  CAMLparam1(v);
  array_type_t* p = (array_type_t*)malloc(sizeof(array_type_t));
  caml_register_global_root(&p->v);
  p->v = v;
  CAMLreturnT(array_type, p);
}

array_type mk_array_type(elt_type elt_type, int rank) {
  CAMLparam1(elt_type);
  CAMLlocal1(t);
  t = caml_callback2(*type_callback_mk_array, elt_type, Val_int(rank));
  CAMLreturnT(array_type, build_array_root(t));
}

void free_type(array_type t) {
  CAMLparam0();
  array_type_t* p = (array_type_t*)t;
  caml_remove_global_root(&p->v);
  free(p);
  CAMLreturn0;
}

int get_type_rank(array_type t) {
  CAMLparam0();
  CAMLreturnT(int,
              Int_val(caml_callback(*type_callback_rank, get_array_value(t))));
}

elt_type get_array_element_type(array_type t) {
  CAMLparam0();
  CAMLreturnT(elt_type,
              caml_callback(*type_callback_elt_type, get_array_value(t)));
}

elt_type get_scalar_element_type(scalar_type t) {
  CAMLparam1(t);
  CAMLreturnT(elt_type, caml_callback(*type_callback_elt_type, t));
}

int type_is_bool(scalar_type t) {
  CAMLparam0();
  assert(type_callback_is_bool); 
  assert(type_inited); 
  CAMLreturnT(int, Bool_val(caml_callback(*type_callback_is_bool, t)));
}

int type_is_char(scalar_type t) {
  CAMLparam0();
  CAMLreturnT(int, Bool_val(caml_callback(*type_callback_is_char, t)));
}

int type_is_int32(scalar_type t) {
  CAMLparam0();
  CAMLreturnT(int, Bool_val(caml_callback(*type_callback_is_int32, t)));
}

int type_is_int64(scalar_type t) {
  CAMLparam0();
  CAMLreturnT(int, Bool_val(caml_callback(*type_callback_is_int64, t)));
}

int type_is_float32(scalar_type t) {
  CAMLparam0();
  CAMLreturnT(int, Bool_val(caml_callback(*type_callback_is_float32, t)));
}

int type_is_float64(scalar_type t) {
  CAMLparam0();
  CAMLreturnT(int, Bool_val(caml_callback(*type_callback_is_float64, t)));
}

/** Private functions **/
static value get_array_value(array_type t) {
  CAMLparam0();
  CAMLlocal1(v);
  v = ((array_type_t*)t)->v;
  CAMLreturn(v);
}
