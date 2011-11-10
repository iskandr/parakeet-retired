#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "type_stubs.h"
#include "variants.h"

int type_inited = 0;

value *get_elt_type_callback = NULL;
value *type_rank_callback = NULL;

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
     get_elt_type_callback = caml_named_value("get_elt_type");
     type_rank_callback = caml_named_value("type_rank");

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


type mk_array(elt_type elt_type, int rank) {
  CAMLparam0();
  CAMLlocal2(ocaml_vec, ocaml_subtype);

  ocaml_vec = alloc(1, VecT);
  caml_register_global_root(&ocaml_vec);
  ocaml_subtype = (value)subtype;
  Store_field(ocaml_vec, 0, ocaml_subtype);
  caml_remove_global_root(&ocaml_subtype);

  CAMLreturnT(type, (type)ocaml_vec);
}

void free_type(array_type t) {
  CAMLparam0();
  CAMLlocal1(ocaml_type);

  ocaml_type = (value)t;
  caml_remove_global_root(&ocaml_type);

  CAMLreturn0;
}

int get_type_rank(array_type t) {
  return Int_val(caml_callback(*type_rank_callback, t));
}


elt_type get_type_element_type(array_type t) {
  CAMLparam1(t);
  return caml_callback(*get_elt_type_callback, t);
}

int type_is_scalar(array_type t) {
  CAMLparam0();
  CAMLlocal1(ocaml_type);
  ocaml_type = (value)t;
  CAMLreturnT(int, Is_long(ocaml_type));
}

