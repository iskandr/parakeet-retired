#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "dyn_type_stubs.h"
#include "variants.h"

int dyn_type_inited = 0;

value *get_elt_type_callback = NULL;
value *type_rank_callback = NULL;

dyn_type parakeet_bool_t;
dyn_type parakeet_char_t;
dyn_type parakeet_int32_t;
dyn_type parakeet_int64_t;
dyn_type parakeet_float32_t;
dyn_type parakeet_float64_t;

void dyn_type_init() {
  if (dyn_type_inited == 0) {
     dyn_type_inited = 1;
     get_elt_type_callback = caml_named_value("get_elt_type");
     type_rank_callback = caml_named_value("type_rank");

     parakeet_bool_t = *caml_named_value("bool_t");
     parakeet_char_t = *caml_named_value("char_t");
     parakeet_int32_t = *caml_named_value("int32_t");
     parakeet_int64_t = *caml_named_value("int64_t");
     parakeet_float32_t = *caml_named_value("float32_t");
     parakeet_float64_t = *caml_named_value("float64_t");
   }
}

/** Public interface **/
/*
dyn_type mk_scalar(dyn_type_no_data_t t) {
  CAMLparam0();
  CAMLlocal1(ocaml_dyn_type);

  caml_register_global_root(&ocaml_dyn_type);
  ocaml_dyn_type = Val_int(t);

  CAMLreturnT(dyn_type, (dyn_type)ocaml_dyn_type);
}
*/

dyn_type mk_vec(dyn_type subtype) {
  CAMLparam0();
  CAMLlocal2(ocaml_vec, ocaml_subtype);

  ocaml_vec = alloc(1, VecT);
  caml_register_global_root(&ocaml_vec);
  ocaml_subtype = (value)subtype;
  Store_field(ocaml_vec, 0, ocaml_subtype);
  caml_remove_global_root(&ocaml_subtype);

  CAMLreturnT(dyn_type, (dyn_type)ocaml_vec);
}

void free_dyn_type(dyn_type t) {
  CAMLparam0();
  CAMLlocal1(ocaml_dyn_type);

  ocaml_dyn_type = (value)t;
  caml_remove_global_root(&ocaml_dyn_type);

  CAMLreturn0;
}

int get_dyn_type_rank(dyn_type t) {
  return Int_val(caml_callback(*type_rank_callback, t));
}


dyn_type get_dyn_type_element_type(dyn_type t) {
  CAMLparam1(t);
  return caml_callback(*get_elt_type_callback, t);
}

int dyn_type_is_scalar(dyn_type t) {
  CAMLparam0();
  CAMLlocal1(ocaml_dyn_type);

  ocaml_dyn_type = (value)t;

  CAMLreturnT(int, Is_long(ocaml_dyn_type));
}
/*
dyn_type get_type_tag(dyn_type t) {
  CAMLparam0();
  CAMLlocal1(ocaml_dyn_type);

  ocaml_dyn_type = (value)t;

  dt_t ret;
  if (Is_long(ocaml_dyn_type)) {
    ret.scalar = 1;
    ret.tag.no_data_tag = Val_int(ocaml_dyn_type);
  } else {
    ret.scalar = 0;
    ret.tag.data_tag = Tag_val(ocaml_dyn_type);
  }

  CAMLreturnT(dt_t, ret);
}
*/
dyn_type get_subtype(dyn_type t) {
  CAMLparam0();
  CAMLlocal2(ocaml_dyn_type, ocaml_subtype);

#ifdef DEBUG
  // We only make this check in debug mode
  if (is_scalar(t)) {
    printf("Attempting to get subtype of scalar type - Aborting.\n");
    exit(1);
  }
#endif

  ocaml_dyn_type = (value)t;
  caml_register_global_root(&ocaml_subtype);
  ocaml_subtype  = Field(ocaml_dyn_type, 0);

  CAMLreturnT(dyn_type, (dyn_type)ocaml_subtype);
}

