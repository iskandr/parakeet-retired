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
#include "type_stubs.h"
#include "value_stubs.h"

#include "front_end_stubs.h"

/** Private members **/
value *ocaml_register_untyped_function = NULL;
value *ocaml_run_function              = NULL;
int fe_inited = 0;

static CAMLprim value build_str_list(char **strs, int num_strs);
static CAMLprim value build_host_val_list(host_val *vals, int num_vals);
static int ocaml_list_length(value l);
static CAMLprim value get_value_and_remove_root(host_val h);

/** Public interface **/

void front_end_init(void) {
  if (fe_inited) return;

  fe_inited = 1;

  ocaml_register_untyped_function =
    caml_named_value("register_untyped_function");
  ocaml_run_function = caml_named_value("run_function");
}

int register_untyped_function(char *name, char **globals, int num_globals,
                              char **args, int num_args, paranode ast) {
  CAMLparam0();
  CAMLlocal5(val_name, val_globals, val_args, val_ast, fn_id);
 
  val_name = caml_copy_string(name);

  val_globals = build_str_list(globals, num_globals);
  val_args    = build_str_list(args, num_args);

  value func_args[4];
  func_args[0] = val_name;
  func_args[1] = val_globals;
  func_args[2] = val_args;
  func_args[3] = (value)ast;

  fn_id = caml_callbackN(*ocaml_register_untyped_function, 4, func_args);

  CAMLreturnT(int, Int_val(fn_id));
}

return_val_t run_function(int id, host_val *globals, int num_globals,
                          host_val *args, int num_args) {
  CAMLparam0();
  CAMLlocal5(ocaml_rslt, ocaml_id, ocaml_globals, ocaml_args, ocaml_ret_type);
  CAMLlocal4(ocaml_shape, ocaml_strides, ocaml_data, ocaml_cur);

  ocaml_id      = Val_int(id);
  ocaml_globals = build_host_val_list(globals, num_globals);
  ocaml_args    = build_host_val_list(args, num_args);

  ocaml_rslt = caml_callback3(*ocaml_run_function, ocaml_id,
                              ocaml_globals, ocaml_args);

  ocaml_cur = Field(ocaml_rslt, 0);
  return_val_t ret;
  ret.results_len = ocaml_list_length(ocaml_cur);
  ret.results = (ret_t*)malloc(sizeof(ret_t) * ret.results_len);

  int i, j;
  for (i = 0; i < ret.results_len; ++i) {
    host_val v = (host_val)Field(ocaml_cur, 0);
    ocaml_cur = Field(ocaml_cur, 1);
    array_type t = value_type_of(v);

    if (Is_long(ocaml_rslt)) {
      // In this case, we know that the return code must have been Pass,
      // since the other two return codes have data.
      ret.return_code = RET_PASS;
      ret.results_len = 0;
    } else if (Tag_val(ocaml_rslt) == RET_SUCCESS) {
      ret.return_code = RET_SUCCESS;

      // returning a scalar
      if (value_is_scalar(v)) {
        ret.results[i].is_scalar = 1;
        ret.results[i].data.scalar.ret_type = get_element_type(t);

        // WARNING:
        // Tiny Memory Leak Ahead
        // -----------------------
        // When scalar data is returned to the host language
        // on the heap, it should be manually deleted by the
        // host frontend
        if (type_is_bool(t)) {
          ret.results[i].data.scalar.ret_scalar_value.boolean = get_bool(v);
        } else if (type_is_int32(t)) {
          ret.results[i].data.scalar.ret_scalar_value.int32 = get_int32(v);
        } else if (type_is_int64(t)) {
          ret.results[i].data.scalar.ret_scalar_value.int64 = get_int64(v);
        } else if (type_is_float32(t)) { 
          ret.results[i].data.scalar.ret_scalar_value.float32 = get_float64(v);
        } else if (type_is_float64(t)) {
          ret.results[i].data.scalar.ret_scalar_value.float64 = get_float64(v);
        } else {
          caml_failwith("Unable to return scalar of this type\n");
        }
      } else {
        // Pass the type
        ret.results[i].is_scalar = 0;
        ret.results[i].data.array.ret_type = t;

        // Pass the data
        ret.results[i].data.array.data = get_array_data(v);

        // Build the shape array
        ocaml_shape = value_get_shape(v);
        int shape_len = Wosize_val(ocaml_shape);

        ret.results[i].data.array.shape =
            (int*)malloc(shape_len * sizeof(int));
        ret.results[i].data.array.shape_len = shape_len;
        for (j = 0; j < shape_len; ++j) {
          ret.results[i].data.array.shape[j] = Int_val(Field(ocaml_shape, j));
        }

        // Build the strides array
        ocaml_strides = value_get_strides(v);
        int strides_len = Wosize_val(ocaml_strides);

        ret.results[i].data.array.strides_len = strides_len;
        ret.results[i].data.array.strides =
            (int*)malloc(strides_len * sizeof(int));
        for (j = 0; j < strides_len; ++j) {
          ret.results[i].data.array.strides[j] =
              Int_val(Field(ocaml_strides, j));
        }
      }
    } else if (Tag_val(ocaml_rslt) == RET_FAIL) {
      ret.return_code = RET_FAIL;
      ret.results_len = caml_string_length(Field(ocaml_rslt, 0));
      ret.error_msg = malloc(ret.results_len);
      strcpy(ret.error_msg, String_val(Field(ocaml_rslt, 0)));
    } else {
      caml_failwith("Unknown return code from run_function. Aborting.");
    }
  }

  CAMLreturnT(return_val_t, ret);
}

void free_return_val(return_val_t ret_val) {
  CAMLparam0();
  CAMLlocal1(ret_type);
  
  int i;

  // Free the results
  if (ret_val.results) {
    for (i = 0; i < ret_val.results_len; ++i) {
      if (!ret_val.results[i].is_scalar) {
        free(ret_val.results[i].data.array.data);
        free(ret_val.results[i].data.array.shape);
        free(ret_val.results[i].data.array.strides);
      }
    }
    free(ret_val.results);
  } else if (ret_val.error_msg) {
    // Free the error msg
    free(ret_val.error_msg);
  }

  CAMLreturn0;
}

/** Private functions **/

static CAMLprim value build_str_list(char **strs, int num_strs) {
  CAMLparam0();
  CAMLlocal3(ocaml_str, cons1, cons2);

  int i;
  if (num_strs > 0) {
    cons1 = caml_alloc_tuple(2);
    ocaml_str = caml_copy_string(strs[num_strs - 1]);
    Store_field(cons1, 0, ocaml_str);
    Store_field(cons1, 1, Val_int(0));

    for (i = num_strs - 2; i >= 0; --i) {
      cons2 = caml_alloc_tuple(2);
      ocaml_str = caml_copy_string(strs[i]);
      Store_field(cons2, 0, ocaml_str);
      Store_field(cons2, 1, cons1);
      cons1 = cons2;
    }
  } else {
    cons1 = Val_int(0);
  }

  CAMLreturn(cons1);
}

static CAMLprim value build_host_val_list(host_val *vals, int num_vals) {
  CAMLparam0();
  CAMLlocal3(ocaml_val, val1, val2);

  int i;

  if (num_vals > 0) {
    val1 = caml_alloc_tuple(2);
    ocaml_val = (value)vals[num_vals - 1];
    Store_field(val1, 0, ocaml_val);
    Store_field(val1, 1, Val_int(0));

    for (i = num_vals - 2; i >= 0; --i) {
      val2 = caml_alloc_tuple(2);
      ocaml_val = (value)vals[i];
      Store_field(val2, 0, ocaml_val);
      Store_field(val2, 1, val1);
      val1 = val2;
    }
  } else {
    val1 = Val_int(0);
  }

  CAMLreturn(val1);
}

static int ocaml_list_length(value l) {
  CAMLparam1(l);
  CAMLlocal1(cur);

  cur = l;
  int i = 0;
  while (cur != Val_int(0)) {
    cur = Field(cur, 1);
    i++;
  }
  
  return i;
}

static CAMLprim value get_value_and_remove_root(host_val h) {
  CAMLparam0();
  CAMLlocal1(val);

  val = (value)h;
  caml_remove_global_root(&val);

  CAMLreturn(val);
}
