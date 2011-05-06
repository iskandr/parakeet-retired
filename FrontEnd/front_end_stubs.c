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

int register_untyped_function(char *name, char **globals, int num_globals,
                              char **args, int num_args, paranode ast) {
  CAMLparam0();
  CAMLlocal5(val_name, val_globals, val_args, val_ast, fn_id);
 
	
	printf("INTERFACE Name: %s",name);

	//int len = strlen(name);
  //val_name = caml_alloc_string(len);
  //memcpy(String_val(val_name), name, len);
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
  CAMLlocal3(ocaml_host_val, ocaml_host_array, ocaml_shape);
  // when returning a scalar
  CAMLlocal1(ocaml_scalar);

  ocaml_id      = Val_int(id);
  ocaml_globals = build_host_val_list(globals, num_globals);
  ocaml_args    = build_host_val_list(args, num_args);

  ocaml_rslt = caml_callback3(*ocaml_run_function, ocaml_id,
                              ocaml_globals, ocaml_args);

  // For now, the interface is such that we assume only one return val
  // can be returned.  We will need to revisit this later - once the
  // front end function in OCaml returns a list this will seg fault.
  return_val_t ret;
  int i;
  
  if (Is_long(ocaml_rslt)) {
    // In this case, we know that the return code must have been Pass,
    // since the other two return codes have data.
    ret.return_code = RET_PASS;
    ret.results_len = 0;
  } else if (Tag_val(ocaml_rslt) == Success) {
      ret.return_code = RET_SUCCESS;

      // TODO: Allow more than one returned result
      ret.results_len = 1;
      ret.shapes = (int**)malloc(sizeof(int*));
      ret.ret_types = (dyn_type*)malloc(sizeof(dyn_type));
      ret.data.results = (void**)malloc(sizeof(void*));

      ocaml_host_val = Field(ocaml_rslt, 0);

      // returning a scalar
      if (Tag_val(ocaml_host_val) != HostArray) {

        // WARNING:
        // Tiny Memory Leak Ahead
        // -----------------------
        // When scalar data is returned to the host language
        // on the heap, it should be manually deleted by the
        // host frontend

        ocaml_scalar = Field(ocaml_host_val, 0);

        void* scalar_data_ptr;

        /* OCAML Accessors
           Long_val(v) returns the long int encoded in value v.
           Int_val(v) returns the int encoded in value v.
           Bool_val(v) returns 0 if v is the Caml boolean false, 1 if v is true

           Wosize_val(v) returns the size of the block v, in words,
                        excluding the header.
           Tag_val(v) returns the tag of the block v.
           String_val(v) returns a pointer to the first byte of the string v
           Double_val(v) returns the floating-point number contained in value v, with type double.
           Int32_val(v) returns the 32-bit integer contained in the int32 v.
           Int64_val(v) returns the 64-bit integer contained in the int64 v.
           Nativeint_val(v) returns the long integer contained in the nativeint v.
         */
        switch (Tag_val(ocaml_scalar)) {
        case PQNUM_BOOL:
          ret.ret_types[0] = BoolT;
          scalar_data_ptr = malloc(sizeof(int*));
          *((int*) scalar_data_ptr) = Bool_val(Field(ocaml_scalar, 0));
          ret.data.results[0] = scalar_data_ptr;
          break;

        case PQNUM_CHAR:
          ret.ret_types[0] = CharT;
          scalar_data_ptr = malloc(sizeof(char*));
          *((char*)scalar_data_ptr) = Int_val(Field(ocaml_scalar, 0));
          ret.data.results[0] = scalar_data_ptr;
          break;

        case PQNUM_INT32:
          ret.ret_types[0] = Int32T;
          scalar_data_ptr = malloc(sizeof(int*));
          *((int*)scalar_data_ptr) = Int32_val(Field(ocaml_scalar, 0));
          ret.data.results[0] = scalar_data_ptr;
          break;

        case PQNUM_FLOAT32:
          ret.ret_types[0] = Float32T;
          scalar_data_ptr = malloc(sizeof(float*));
          *((float*)scalar_data_ptr) = Double_val(Field(ocaml_scalar, 0));
          ret.data.results[0] = scalar_data_ptr;
          break;

        case PQNUM_FLOAT64:
          ret.ret_types[0] = Float64T;
          scalar_data_ptr = malloc(sizeof(double*));
          *((double*)scalar_data_ptr) = Double_val(Field(ocaml_scalar, 0));
          ret.data.results[0] = scalar_data_ptr;
          break;

        case PQNUM_UINT16:
        case PQNUM_INT16:
        case PQNUM_UINT32:
        case PQNUM_UINT64:
        case PQNUM_INT64:
        case PQNUM_INF:
        case PQNUM_NEGINF:
          caml_failwith("Unable to return scalar of this type\n");
        }
      } else {
        ocaml_host_array = Field(ocaml_host_val, 0);

        // I think we only need to register a global root here for the dyn_type,
        // since the return value's data was created in C world and is just a
        // pointer to something outside the OCaml heap.
        ret.ret_types[0] = (dyn_type)Field(ocaml_host_array, HostArray_HOST_T);

        // Build the results array
        ret.data.results[0] =
            (void*)Int64_val(Field(ocaml_host_array, HostArray_PTR));

        // Build the shapes array
        ocaml_shape = Field(ocaml_host_array, HostArray_SHAPE);
        int shape_len = Wosize_val(ocaml_shape);

        ret.shapes[0] = (int*)malloc(shape_len);
        for (i = 0; i < shape_len; ++i) {
          ret.shapes[0][i] = Int_val(Field(ocaml_shape, i));
        }
      }
  } else if (Tag_val(ocaml_rslt) == Error) {
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

