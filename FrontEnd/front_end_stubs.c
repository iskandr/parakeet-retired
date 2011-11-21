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
 
	
  printf("INTERFACE Name: %s",name);

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
  // when returning an array
  CAMLlocal3(ocaml_shape, ocaml_strides, ocaml_data);

  ocaml_id      = Val_int(id);
  ocaml_globals = build_host_val_list(globals, num_globals);
  ocaml_args    = build_host_val_list(args, num_args);

  ocaml_rslt = caml_callback3(*ocaml_run_function, ocaml_id,
                              ocaml_globals, ocaml_args);

  return_val_t ret;
  host_val v = (host_val)Field(ocaml_rslt, 0);
  int i;

  // TODO: Support multiple return values.  All that needs to be done is
  //       to add support for that to OCaml, and then loop over all the values
  //       returned with the following code.
  ret.results_len = 1;
  ret.shapes = (int**)malloc(sizeof(int*));
  ret.ret_types = (array_type*)malloc(sizeof(array_type));
  ret.data.results = (void**)malloc(sizeof(void*));
  array_type t = value_type_of(v);
  ret.ret_types[0] = t;

  if (Is_long(ocaml_rslt)) {
    // In this case, we know that the return code must have been Pass,
    // since the other two return codes have data.
    ret.return_code = RET_PASS;
    ret.results_len = 0;
  } else if (Tag_val(ocaml_rslt) == RET_SUCCESS) {
      ret.return_code = RET_SUCCESS;

      // returning a scalar
      if (value_is_scalar(v)) {
        
        // WARNING:
        // Tiny Memory Leak Ahead
        // -----------------------
        // When scalar data is returned to the host language
        // on the heap, it should be manually deleted by the
        // host frontend
        
        if (t == parakeet_bool_t) {
          ret.data.results[0] = malloc(sizeof(int));
          *((int*)ret.data.results[0]) = get_bool(v);
        } else if (t == parakeet_int32_t) {
          ret.data.results[0] = malloc(sizeof(int32_t));
          *((int32_t*)ret.data.results[0]) = get_int32(v);
        } else if (type_is_int64) {
          ret.data.results[0] = malloc(sizeof(int64_t));
          *((int64_t*)ret.data.results[0]) = get_int64(v);
        } else if (type_is_float64) {
          ret.data.results[0] = malloc(sizeof(double));
          *((double*)ret.data.results[0]) = get_float64(v);
        } else {
          caml_failwith("Unable to return scalar of this type\n");
        }
      } else {
        // Build the results array
        ret.data.results[0] = get_array_data(v);

        // Build the shapes array
        ocaml_shape = value_get_shape(v);
        int shape_len = Wosize_val(ocaml_shape);

        ret.shapes[0] = (int*)malloc(shape_len);
        for (i = 0; i < shape_len; ++i) {
          ret.shapes[0][i] = Int_val(Field(ocaml_shape, i));
        }
        
        // Build the strides array
        ocaml_strides = value_get_strides(v);
        int strides_len = Wosize_val(ocaml_strides);
        
        ret.strides[0] = (int*)malloc(strides_len);
        for (i = 0; i < strides_len; ++i) {
          ret.strides[0][i] = Int_val(Field(ocaml_strides, i));
        }
      }
  } else if (Tag_val(ocaml_rslt) == RET_FAIL) {
    ret.return_code = RET_FAIL;
    ret.results_len = caml_string_length(Field(ocaml_rslt, 0));
    ret.data.error_msg = malloc(ret.results_len);
    strcpy(ret.data.error_msg, String_val(Field(ocaml_rslt, 0)));
  } else {
    caml_failwith("Unknown return code from run_function. Aborting.");
  }

  CAMLreturnT(return_val_t, ret);
}

void free_return_val(return_val_t ret_val) {
  CAMLparam0();
  CAMLlocal1(ret_type);
  
  int i;

  // Free the return types
  free(ret_val.ret_types);

  // Free the shapes
  for (i = 0; i < ret_val.results_len; ++i) {
    free(ret_val.shapes[i]);
  }
  free(ret_val.shapes);

  // Free error msg if necessary
  if (ret_val.return_code == RET_FAIL) {
    free(ret_val.data.error_msg);
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

static CAMLprim value get_value_and_remove_root(host_val h) {
  CAMLparam0();
  CAMLlocal1(val);

  val = (value)h;
  caml_remove_global_root(&val);

  CAMLreturn(val);
}
