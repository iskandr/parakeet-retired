#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "dyn_type_stubs.h"
#include "variants.h"

/** Public interface **/
dyn_type mk_scalar(dyn_type_no_data_t t) {
  CAMLparam0();
  CAMLlocal1(ocaml_dyn_type);

  caml_register_global_root(&ocaml_dyn_type);
  ocaml_dyn_type = Val_int(t);

  CAMLreturnT(dyn_type, (dyn_type)ocaml_dyn_type);
}

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
  int rank = 0;
  dyn_type cur = t;
  while(!dyn_type_is_scalar(cur)) {
    rank++;
    cur = get_subtype(cur);
  }

  return rank;
}

dyn_type_no_data_t get_dyn_type_element_type(dyn_type t) {
  dyn_type cur = t;
  while(!dyn_type_is_scalar(cur)) {
    cur = get_subtype(cur);
  }

  return (dyn_type_no_data_t)Val_int(cur);
}

int dyn_type_is_scalar(dyn_type t) {
  CAMLparam0();
  CAMLlocal1(ocaml_dyn_type);

  ocaml_dyn_type = (value)t;

  CAMLreturnT(int, Is_long(ocaml_dyn_type));
}

dt_t get_type_tag(dyn_type t) {
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

