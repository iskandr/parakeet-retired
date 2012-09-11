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
value *ocaml_get_adverb                = NULL; 
value *ocaml_run_adverb                = NULL; 
value *ocaml_set_vectorize             = NULL;
value *ocaml_set_multithreading        = NULL;
int fe_inited = 0;


value build_host_val_list(host_val *vals, int num_vals);
value build_int_list(int* nums, int count);

int ocaml_list_length(value l);

/** Public interface **/

void front_end_init(void) {
  if (fe_inited) return;

  fe_inited = 1;

  ocaml_register_untyped_function =
    caml_named_value("register_untyped_function");
  ocaml_run_function = caml_named_value("run_function");
  ocaml_set_vectorize = caml_named_value("set_vectorize");
  ocaml_set_multithreading = caml_named_value("set_multithreading");
  ocaml_get_adverb = caml_named_value("get_adverb");
  ocaml_run_adverb = caml_named_value("run_adverb");
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
  CAMLlocal1(fn_id);

  printf(":: registering untyped function (%s, %d globals, %d args)\n",
    name, num_globals, num_args);

  val_name = caml_copy_string(name);
  printf("::: building globals list");
  globals_list = build_str_list(globals, num_globals);
  printf("::: building args list");
  args_list    = build_str_list(args, num_args);
  printf("::: building defaults list");
  default_arg_names_list = build_str_list(default_args, num_defaults);

  printf("::: building default values list");
  default_arg_values_list = mk_val_list(default_arg_values, num_defaults);
  value func_args[6] = {
    val_name,
    globals_list,
    args_list,
    default_arg_names_list, 
    default_arg_values_list,
    ast->v
  };

  printf("  ...calling into OCaml's register function\n");
  fn_id = caml_callbackN(*ocaml_register_untyped_function, 6, func_args);
  printf("DONE WITH FN ID: %d\n", Int_val(fn_id));
  CAMLreturnT(int, Int_val(fn_id));
}


/* given a return value in OCaml land, translate it to 
   the return_val_t C structure
*/ 
return_val_t translate_return_value(value ocaml_result) {
  CAMLparam1(ocaml_result);
  CAMLlocal5(ocaml_shape, ocaml_strides, ocaml_data, ocaml_cur, ocaml_type);
  CAMLlocal1(v);
  
  return_val_t ret;
  
  if (Is_long(ocaml_result)) {
    // In this case, we know that the return code must have been Pass,
    // since the other two return codes have data.
    ret.return_code = RET_PASS;
    ret.results_len = 0;
  } else if (Tag_val(ocaml_result) == RET_FAIL) {
    ret.return_code = RET_FAIL;
    ret.results_len = caml_string_length(Field(ocaml_result, 0));
    ret.error_msg = malloc(ret.results_len + 1);
    strcpy(ret.error_msg, String_val(Field(ocaml_result, 0)));
  } else if (Tag_val(ocaml_result) == RET_SUCCESS) {
    
    ocaml_cur = Field(ocaml_result, 0);
    ret.return_code = RET_SUCCESS;
    ret.results_len = ocaml_list_length(ocaml_cur);
    ret.results = (ret_t*)malloc(sizeof(ret_t) * ret.results_len);
    
    int i, j;
    host_val h; 
    for (i = 0; i < ret.results_len; ++i) {
      v = Field(ocaml_cur, 0);
      h = create_host_val(v);  
      ocaml_cur = Field(ocaml_cur, 1);
      // returning a scalar
      if (value_is_scalar(h)) {

        ret.results[i].is_scalar = 1;
        ocaml_type = (scalar_type)value_type_of(h);
        ret.results[i].data.scalar.ret_type =
            get_scalar_element_type(ocaml_type);

        // WARNING:
        // Tiny Memory Leak Ahead
        // -----------------------
        // When scalar data is returned to the host language
        // on the heap, it should be manually deleted by the
        // host frontend

        if (type_is_bool(ocaml_type)) {
          ret.results[i].data.scalar.ret_scalar_value.boolean = get_bool(h);
        } else if (type_is_int32(ocaml_type)) {
          ret.results[i].data.scalar.ret_scalar_value.int32 = get_int32(h);
        } else if (type_is_int64(ocaml_type)) {
          ret.results[i].data.scalar.ret_scalar_value.int64 = get_int64(h);
        } else if (type_is_float32(ocaml_type)) { 
          ret.results[i].data.scalar.ret_scalar_value.float32 = get_float64(h);
        } else if (type_is_float64(ocaml_type)) {
          ret.results[i].data.scalar.ret_scalar_value.float64 = get_float64(h);
        } else {
          caml_failwith("Unable to return scalar of this type\n");
        }
      } else {
        // Pass the type
        ret.results[i].is_scalar = 0;
        ret.results[i].data.array.ret_type = array_type_of(h);

        // Pass the data
        ret.results[i].data.array.data = get_array_data(h);

        // Build the shape array
        ocaml_shape = value_get_shape(h);
        int shape_len = Wosize_val(ocaml_shape);

        ret.results[i].data.array.shape =
            (int*)malloc(shape_len * sizeof(int));
        ret.results[i].data.array.shape_len = shape_len;
        for (j = 0; j < shape_len; ++j) {
          ret.results[i].data.array.shape[j] = Int_val(Field(ocaml_shape, j));
        }

        // Build the strides array
        ocaml_strides = value_get_strides(h);
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
  CAMLreturnT(return_val_t, ret);
	
}

value get_adverb(char* adverb_name) {
  CAMLparam0();
  CAMLlocal1(ocaml_adverb_name);
  ocaml_adverb_name = caml_copy_string(adverb_name); 
  CAMLreturn(caml_callback(*ocaml_get_adverb, ocaml_adverb_name));
}

value mk_actual_args(
          host_val *args, 
          int num_args,
          char** keywords,
          host_val* keyword_values,
          int num_keyword_args) {
  CAMLparam0(); 
  CAMLlocal3(pos_list, kwd_list, kwd_values_list);
  CAMLlocal2(actual_args, ocaml_fn);
  printf("Creating host_val args, n_positional = %d, n_kwd = %d\n", num_args, num_keyword_args);
  pos_list = build_host_val_list(args, num_args);
  kwd_list = build_str_list(keywords, num_keyword_args);
  kwd_values_list = build_host_val_list(keyword_values, num_keyword_args);
  ocaml_fn = *caml_named_value("mk_actual_args"); 
  actual_args = \
    caml_callback3(ocaml_fn, pos_list, kwd_list, kwd_values_list);
  CAMLreturn(actual_args);
}

return_val_t run_adverb(
  char* adverb_name, 
  int fn_id, 
  host_val* globals, int num_globals,
  host_val* fixed, int num_fixed,
  char **fixed_keyword_names, host_val* fixed_keyword_values, int num_fixed_keywords, 
  host_val* init, int num_init, 
  int axes_given, int* axes, int num_axes, 
  host_val* array_positional, int num_array_positional,
  char** array_keyword_names, host_val* array_keyword_values, int num_array_keyword_values) {
   
  CAMLparam0();
  CAMLlocal2(adverb, globals_list);
  CAMLlocal2(fixed_actuals, array_actuals);
  CAMLlocal2(init_list, axes_list_option); 
  CAMLlocal1(ocaml_result);
  
  printf("Building list of %d global values\n", num_globals); 
  globals_list = build_host_val_list(globals, num_globals);
  printf("Making fixed args from %d fixed values and %d fixed kwds\n", 
    num_fixed, num_fixed_keywords); 
  fixed_actuals = mk_actual_args(fixed, num_fixed, \
    fixed_keyword_names, fixed_keyword_values, num_fixed_keywords);
  printf("Making array args from %d arrays and %d kwds\n", 
    num_array_positional, num_array_keyword_values); 
  array_actuals = mk_actual_args(array_positional, num_array_positional, \
    array_keyword_names, array_keyword_values, num_array_keyword_values); 
  printf("Building list of %d init args\n", num_init); 
  init_list = build_host_val_list(init, num_init);
  printf("Axes given? %d\n", axes_given);
  if (axes_given) {
    printf("Building %d axes\n", num_axes); 
    axes_list_option = caml_alloc(1, 0);
    Store_field( axes_list_option, 0,  build_int_list(axes, num_axes) );
  } else {
    axes_list_option = Val_int(0);
  }
  printf("Calling into OCaml\n");  
  adverb = get_adverb(adverb_name); 
  value func_args[7] = {
    adverb,
    Val_int(fn_id), 
    globals_list, 
    init_list, 
    axes_list_option, 
    fixed_actuals,
    array_actuals
  };
  ocaml_result = caml_callbackN(*ocaml_run_adverb, 7, func_args);
  printf("Returned from OCaml\n"); 
  CAMLreturnT(return_val_t, translate_return_value(ocaml_result));

}

return_val_t run_function(int id, 
    host_val *globals, int num_globals,
    host_val *args, int num_args, 
    char** kwd_arg_names, host_val* kwd_arg_values, int num_kwd_args) {
  CAMLparam0();
  CAMLlocal3(ocaml_globals, ocaml_actuals, ocaml_result);
  printf("[run_function] %d globals, %d args, %d kwd args\n", num_globals, num_args, num_kwd_args);
  ocaml_globals = build_host_val_list(globals, num_globals);
  ocaml_actuals = mk_actual_args(args, num_args, kwd_arg_names, kwd_arg_values, num_kwd_args); 
  ocaml_result = caml_callback3(*ocaml_run_function, Val_int(id), ocaml_globals, ocaml_actuals);
  CAMLreturnT(return_val_t, translate_return_value(ocaml_result));
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

value build_host_val_list(host_val *vals, int num_vals) {
  CAMLparam0();
  CAMLlocal3(old_tail, new_tail, elt); 
  old_tail = Val_int(0); 
  int i;
  for (i = num_vals - 1; i >= 0; i--) { 
    elt = host_val_contents(vals[i]); 
    new_tail = caml_alloc_tuple(2); 
    Store_field(new_tail, 0, elt); 
    Store_field(new_tail, 1, old_tail); 
    old_tail = new_tail;
  }
  CAMLreturn(old_tail); 
}

value build_int_list(int* nums, int count) {
  CAMLparam0();
  CAMLlocal2(old_tail, new_tail);
  old_tail = Val_int(0);
  int i; 
  for (i = count - 1; i >= 0; --i) {
    new_tail = caml_alloc_tuple(2);
    Store_field(new_tail, 0, Val_int(nums[i]));
    Store_field(new_tail, 1, old_tail);
    old_tail = new_tail; 
  }
  CAMLreturn(old_tail);
}

int ocaml_list_length(value l) {
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
