/*
 *  front_end_stubs.c
 *
 *  Front end functions for registering and running functions with the Parakeet
 *  runtime.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2012.
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
value *ocaml_set_vectorize             = NULL;
value *ocaml_set_multithreading        = NULL;
int fe_inited = 0;

static CAMLprim value build_str_list(char **strs, int num_strs);
static CAMLprim value build_host_val_list(host_val *vals, int num_vals);
static int ocaml_list_length(value l);

/** Public interface **/

void front_end_init(void) {
  if (fe_inited) return;

  fe_inited = 1;

  ocaml_register_untyped_function =
    caml_named_value("register_untyped_function");
  ocaml_run_function = caml_named_value("run_function");
  ocaml_set_vectorize = caml_named_value("set_vectorize");
  ocaml_set_multithreading = caml_named_value("set_multithreading");
}

/*     LibPar.register_untyped_function(
      fn_name_c_str,
      globals_array, n_globals,
      postional_args_array, n_positional,
      default_args_array, default_values_array, n_defaults,
      parakeet_syntax))
      */
int register_untyped_function(
		char *name,
		char **globals, int num_globals,
        char **args, int num_args,
        char **default_args, paranode *default_arg_values, int num_defaults,
        paranode ast) {

  CAMLparam0();

  CAMLlocal3(val_name, globals_list, args_list);
  CAMLlocal2(default_arg_names_list, default_arg_values_list);
  CAMLlocal2(val_ast, fn_id);

  printf(":: registering untyped function\n");

  printf("  ...copying function name\n");
  val_name = caml_copy_string(name);

  printf("  ...copying global names\n");
  globals_list = build_str_list(globals, num_globals);
  printf("  ...copying arg names\n");
  args_list    = build_str_list(args, num_args);

  printf("  ...copying default arg names\n");
  default_arg_names_list = build_str_list(default_args, num_defaults);

  printf("  ...copying default arg values\n");
  default_arg_values_list = mk_val_list(default_arg_values, num_defaults);
  val_ast = ast->v;
  value func_args[6] = {
    val_name, globals_list, args_list,
	default_arg_names_list, default_arg_values_list,
	val_ast
  };

  printf("  ...calling into OCaml's register function\n");
  fn_id = caml_callbackN(*ocaml_register_untyped_function, 6, func_args);

  CAMLreturnT(int, Int_val(fn_id));
}

return_val_t run_function(int id, 
    host_val *globals, int num_globals,
    host_val *args, int num_args, 
    char** kwd_arg_names, host_val* kwd_arg_values, int num_kwd_args) {
  CAMLparam0();
  CAMLlocal5(ocaml_rslt, ocaml_id, ocaml_globals, ocaml_args, ocaml_ret_type);
  CAMLlocal5(ocaml_shape, ocaml_strides, ocaml_data, ocaml_cur, ocaml_type);
  CAMLlocal3(v, ocaml_kwd_names, ocaml_kwd_args); 

  ocaml_id      = Val_int(id);
  ocaml_globals = build_host_val_list(globals, num_globals);
  ocaml_args    = build_host_val_list(args, num_args);
  ocaml_kwd_names = build_str_list(kwd_arg_names, num_kwd_args);
  ocaml_kwd_args = build_host_val_list(kwd_arg_values, num_kwd_args);

  value func_args[5] = {
		  ocaml_id, ocaml_globals, ocaml_args, ocaml_kwd_names, ocaml_kwd_args};
  ocaml_rslt = caml_callbackN(*ocaml_run_function, 5, func_args);

  ocaml_cur = Field(ocaml_rslt, 0);
  return_val_t ret;

  if (Is_long(ocaml_rslt)) {
    // In this case, we know that the return code must have been Pass,
    // since the other two return codes have data.
    ret.return_code = RET_PASS;
    ret.results_len = 0;
  } else if (Tag_val(ocaml_rslt) == RET_FAIL) {
    ret.return_code = RET_FAIL;
    ret.results_len = caml_string_length(Field(ocaml_rslt, 0));
    ret.error_msg = malloc(ret.results_len);
    strcpy(ret.error_msg, String_val(Field(ocaml_rslt, 0)));
  } else if (Tag_val(ocaml_rslt) == RET_SUCCESS) {
    ret.return_code = RET_SUCCESS;
    ret.results_len = ocaml_list_length(ocaml_cur);
    ret.results = (ret_t*)malloc(sizeof(ret_t) * ret.results_len);
    int i, j;
    for (i = 0; i < ret.results_len; ++i) {
      v = Field(ocaml_cur, 0);
      ocaml_cur = Field(ocaml_cur, 1);
      // returning a scalar
      if (value_is_scalar(v)) {
         
        ret.results[i].is_scalar = 1;
        ocaml_type = (scalar_type)value_type_of(v);
        ret.results[i].data.scalar.ret_type =
            get_scalar_element_type(ocaml_type);

        // WARNING:
        // Tiny Memory Leak Ahead
        // -----------------------
        // When scalar data is returned to the host language
        // on the heap, it should be manually deleted by the
        // host frontend
        
        if (type_is_bool(ocaml_type)) {
          ret.results[i].data.scalar.ret_scalar_value.boolean = get_bool(v);
        } else if (type_is_int32(ocaml_type)) {
          ret.results[i].data.scalar.ret_scalar_value.int32 = get_int32(v);
        } else if (type_is_int64(ocaml_type)) {
          ret.results[i].data.scalar.ret_scalar_value.int64 = get_int64(v);
        } else if (type_is_float32(ocaml_type)) { 
          ret.results[i].data.scalar.ret_scalar_value.float32 = get_float64(v);
        } else if (type_is_float64(ocaml_type)) {
          ret.results[i].data.scalar.ret_scalar_value.float64 = get_float64(v);
        } else {
          caml_failwith("Unable to return scalar of this type\n");
        }
      } else {
        // Pass the type
        ret.results[i].is_scalar = 0;
        ret.results[i].data.array.ret_type = array_type_of(v);

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
    }
  }
  printf("Done!\n"); 
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
        free_type(ret_val.results[i].data.array.ret_type);
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

/** Parakeet parameter configuration **/
void set_vectorize(int val) {
  CAMLparam0();
  CAMLlocal1(ocaml_bool);

  ocaml_bool = Val_bool(val);
  caml_callback(*ocaml_set_vectorize, ocaml_bool);

  CAMLreturn0;
}

void set_multithreading(int val) {
  CAMLparam0();
  CAMLlocal1(ocaml_bool);

  ocaml_bool = Val_bool(val);
  caml_callback(*ocaml_set_multithreading, ocaml_bool);

  CAMLreturn0;
}

/** Private functions **/

static CAMLprim value build_str_list(char **strs, int num_strs) {
  CAMLparam0();
  CAMLlocal3(ocaml_str, cons1, cons2);

  int i;
  if (num_strs > 0) {
    ocaml_str = caml_copy_string(strs[num_strs - 1]);
    cons1 = caml_alloc_tuple(2);
    Store_field(cons1, 0, ocaml_str);
    Store_field(cons1, 1, Val_int(0));

    for (i = num_strs - 2; i >= 0; --i) {
      ocaml_str = caml_copy_string(strs[i]);
      cons2 = caml_alloc_tuple(2);
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
  CAMLlocal3(elt, val1, val2);

  int i;

  if (num_vals > 0) {
    elt = (value)vals[num_vals - 1];
    val1 = caml_alloc_tuple(2);
    Store_field(val1, 0, elt);
    Store_field(val1, 1, Val_int(0));

    for (i = num_vals - 2; i >= 0; --i) {
      elt = (value)vals[i];
      val2 = caml_alloc_tuple(2);
      Store_field(val2, 0, elt);
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
  
  CAMLreturnT(int, i);
}
