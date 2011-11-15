#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "type_stubs.h"
#include "variants.h"

// PRIVATE

int type_inited = 0;

value *type_callback_elt_type = NULL;
value *type_callback_rank = NULL;
value *type_callback_mk_array = NULL;
value *type_callback_is_scalar = NULL;


// PUBLIC
array_type parakeet_bool_t;
elt_type parakeet_bool_elt_t;

array_type parakeet_char_t;
elt_type parakeet_char_elt_t;

array_type parakeet_int32_t;
elt_type parakeet_int32_elt_t;

array_type parakeet_int64_t;
elt_type parakeet_int64_elt_t;

array_type parakeet_float32_t;
elt_type parakeet_float32_elt_t;

array_type parakeet_float64_t;
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

     parakeet_char_t = *caml_named_value("char_t");
     parakeet_char_elt_t = *caml_named_value("char_elt_t");

     parakeet_int32_t = *caml_named_value("int32_t");
     parakeet_int32_elt_t = *caml_named_value("int32_elt_t");

     parakeet_int64_t = *caml_named_value("int64_t");
     parakeet_int64_elt_t = *caml_named_value("int64_elt_t");

     parakeet_float32_t = *caml_named_value("float32_t");
     parakeet_float32_elt_t = *caml_named_value("float32_elt_t");

     parakeet_float64_t = *caml_named_value("float64_t");
     parakeet_float64_elt_t = *caml_named_value("float64_elt_t");
   }
}

/** Public interface **/
array_type mk_array_type(elt_type elt_type, int rank) {
  CAMLparam0();
  CAMLlocal1(arrT);
  arrT = caml_callback2(*type_callback_mk_array, elt_type, Val_int(rank));
  caml_register_global_root(&arrT);
  caml_remove_global_root(&elt_type);
  CAMLreturn(arrT);
}

void free_type(array_type t) {
  CAMLparam0();
  caml_remove_global_root(&t);
  CAMLreturn0;
}

int get_type_rank(array_type t) {
  return Int_val(caml_callback(*type_callback_rank, t));
}


elt_type get_type_element_type(array_type t) {
  CAMLparam1(t);
  CAMLreturn(caml_callback(*type_callback_elt_type, t));
}

int type_is_scalar(array_type t) {
  CAMLparam0();
  CAMLlocal1(b);
  b = caml_callback(*type_callback_is_scalar, t);
  CAMLreturnT(int, Int_val(b));
}

