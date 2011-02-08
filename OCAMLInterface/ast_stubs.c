#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <string.h>

#include "ast_stubs.h"
#include "../OCAMLInterface/variants.h"

paranode mk_lam(char **args, int num_args, paranode body,
                source_info_t *src_info = NULL) {
  CAMLparam1(body);
  CAMLlocal1(lam);

}
  
paranode mk_var(char *str, source_info_t *src_info = NULL) {
  CAMLparam0();
  CAMLlocal3(ocaml_str, var, node);

  // copy over the string
  int len = strlen(str);
  ocaml_str = caml_alloc_string(len);
  memcpy(String_val(ocaml_str), str, len);

  // build the var expression
  var = caml_alloc_tuple(2);
  Store_field(var, 0, Exp_Var);
  Store_field(var, 1, ocaml_str);

  // build the node
  caml_register_global_root(&node);
  

  // return a paranode
  paranode ret = (void*)&node;
  CAMLreturnT(paranode, ret);
}
