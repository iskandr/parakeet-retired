/*
 *  front_end_stubs.c
 *
 *  Front end functions for registering and running functions with the Parakeet
 *  runtime.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2011.
 */

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <string.h>

#include "ast_stubs.h"
#include "dyn_type_stubs.h"
#include "front_end_stubs.h"
#include "host_val_stubs.h"

/** Private members **/
value *ocaml_register_untyped_function = NULL;
value *ocaml_run_function              = NULL;
int fe_inited = 0;
static CAMLprim value build_str_list(char **strs, int num_strs);
static CAMLprim value build_host_val_list(host_val *vals, int num_vals);
static CAMLprim value get_value_and_remove_root(host_val h);

/** Public interface **/

void front_end_init(void) {
  if (fe_inited) return;

  fe_inited = 1;

  ocaml_register_untyped_function =
    caml_named_value("register_untyped_function");
  ocaml_run_function = caml_named_value("run_function");
}

int64_t register_untyped_function(char *name, char **globals, int num_globals,
                                  char **args, int num_args, paranode ast) {
  CAMLparam0();
  CAMLlocal5(val_name, val_globals, val_args, val_ast, fn_id);

  int len;

  printf("Registering bitches\n");
  fflush(stdout);

  len = strlen(name);
  val_name = caml_alloc_string(len);
  memcpy(String_val(val_name), &name, len);

  printf("copied string\n");

  val_globals = build_str_list(globals, num_globals);
  printf("built %d globals\n", num_globals);
  val_args    = build_str_list(args, num_args);
  printf("built %d args\n", num_args);

  printf("ast is at address %p\n", ast);
  value func_args[4];
  func_args[0] = val_name;
  func_args[1] = val_globals;
  func_args[2] = val_args;
  func_args[3] = *(value*)ast;

  printf("calling callback at address %p\n", ocaml_register_untyped_function);
  fflush(stdout);
  fn_id = caml_callbackN(*ocaml_register_untyped_function, 4, func_args);
  printf("registered function\n");

  free(func_args);

  CAMLreturnT(int64_t, Int64_val(fn_id));
}

return_val_t run_function(int64_t id, host_val *globals, int num_globals,
                          host_val *args, int num_args) {
  CAMLparam0();
  CAMLlocal5(ocaml_rslt, ocaml_id, ocaml_globals, ocaml_args, ocaml_ret_type);

  ocaml_id      = copy_int64(id);
  ocaml_globals = build_host_val_list(globals, num_globals);
  ocaml_args    = build_host_val_list(args, num_args);

  ocaml_rslt = caml_callback3(*ocaml_run_function, ocaml_id,
                              ocaml_globals, ocaml_args);

  // For now, the interface is such that we assume only one return val
  // can be returned.  We may need to revisit this later.
  return_val_t ret;
  int msg_len;
  
  if (Is_long(ocaml_rslt)) {
    // In this case, we know that the return code must have been Pass,
    // since the other two return codes have data.
    ret.return_code = RET_PASS;
    ret.num_results = 0;
  } else if (Tag_val(ocaml_rslt) == Success) {
    // I think we only need to register a global root here for the dyn_type,
    // since the return value's data was created in C world and is just a
    // pointer to something outside the OCaml heap.
    ret.return_code = RET_SUCCESS;
    ret.num_results = 1;
    caml_register_global_root(&ocaml_ret_type);
    ocaml_ret_type = Field(ocaml_rslt, 1);
    ret.ret_type = (dyn_type)&ocaml_ret_type;
    ret.data.results = (void*)Int64_val(Field(ocaml_rslt, 0));
  } else if (Tag_val(ocaml_rslt) == Error) {
    ret.return_code = RET_FAIL;
    ret.num_results = 0;
    msg_len = caml_string_length(Field(ocaml_rslt, 0));
    if (msg_len < RET_MSG_MAX_LEN) {
      strcpy(ret.data.error_msg, String_val(Field(ocaml_rslt, 0)));
    } else {
      memcpy(ret.data.error_msg, String_val(Field(ocaml_rslt, 0)),
             RET_MSG_MAX_LEN - 1);
      ret.data.error_msg[RET_MSG_MAX_LEN - 1] = '\0';
    }
  } else {
    printf("Unknown return code from run_function. Aborting.\n");
    exit(1);
  }

  CAMLreturnT(return_val_t, ret);
}

/** Private functions **/

static CAMLprim value build_str_list(char **strs, int num_strs) {
  CAMLparam0();
  CAMLlocal3(ocaml_str, str1, str2);

  int len, i;

  if (num_strs > 0) {
    str1 = caml_alloc_tuple(2);
    len  = strlen(strs[num_strs - 1]);
    ocaml_str = caml_alloc_string(len);
    memcpy(String_val(ocaml_str), &strs[num_strs - 1], len);
    Store_field(str1, 0, ocaml_str);
    Store_field(str1, 1, Val_int(0));

    for (i = num_strs - 2; i >= 0; --i) {
      str2 = caml_alloc_tuple(2);
      len = strlen(strs[i]);
      ocaml_str = caml_alloc_string(len);
      memcpy(String_val(ocaml_str), &strs[i], len);
      Store_field(str2, 0, ocaml_str);
      Store_field(str2, 1, str1);
      str1 = str2;
    }
  } else {
    str1 = Val_int(0);
  }

  CAMLreturn(str1);
}

static CAMLprim value build_host_val_list(host_val *vals, int num_vals) {
  CAMLparam0();
  CAMLlocal3(ocaml_val, val1, val2);

  int i;

  if (num_vals > 0) {
    val1 = caml_alloc_tuple(2);
    ocaml_val = *(value*)vals[num_vals - 1];
    Store_field(val1, 0, ocaml_val);
    Store_field(val1, 1, Val_int(0));

    for (i = num_vals - 2; i >= 0; --i) {
      val2 = caml_alloc_tuple(2);
      ocaml_val = *(value*)vals[i];
      Store_field(val2, 0, ocaml_val);
      Store_field(val2, 1, val1);
      val1 = val2;
    }
  } else {
    val1 = Val_int(0);
  }

  CAMLreturn(val1);
}

static CAMLprim value get_value_and_remove_root(host_val h) {
  CAMLparam0();
  CAMLlocal1(val);

  val = *(value*)h;
  caml_remove_global_root(&val);

  CAMLreturn(val);
}

