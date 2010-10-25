/*
 *  cinterface.c
 *
 * Interface between the OCaml and Q runtimes.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2010.
 */

// TODO: Look into possibly using the lower-level interface for allocating
//       OCaml memory blocks.

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cinterface.h"
#include "k.h"
#include "../OCAMLInterface/ocaml_functions.h"

#define INITIAL_VECTOR_SIZE 8

/** Whether we've done initialization **/
int init = 0;

/** OCaml functions we use **/
value *ocaml_compiler_init          = NULL;
value *ocaml_gen_module_template    = NULL;
value *ocaml_get_function_template  = NULL;
value *ocaml_run_template           = NULL;
value *ocaml_ktypenum_to_ocaml_type = NULL;
value *ocaml_dyn_type_to_ktypenum   = NULL;
value *ocaml_sizeof_dyn_type        = NULL;
value *ocaml_shape_rank             = NULL;
value *ocaml_compact                = NULL;

/** Vectors to store registered OCaml root variables for templates **/
/** TODO: Change this to something better? **/
value *module_templates   = NULL;
value *function_templates = NULL;
int sizeof_module_templates;
int num_module_templates;
int sizeof_function_templates;
int num_function_templates;

void init_dt() {
  CAMLparam0();

  if (init) {
    CAMLreturn0;
  }
  init = 1;

  // Initialize OCaml
  char *argv[] = {"argv", NULL};
  caml_startup(argv);

#ifndef DEBUG
  ocaml_compiler_init          = caml_named_value("compiler_init");
  ocaml_gen_module_template    = caml_named_value("gen_module_template");
  ocaml_get_function_template  = caml_named_value("get_function_template");
  ocaml_run_template           = caml_named_value("run_template");
  ocaml_ktypenum_to_ocaml_type = caml_named_value("ktypenum_to_ocaml_type");
  ocaml_dyn_type_to_ktypenum   = caml_named_value("dyn_type_to_ktypenum");
  ocaml_sizeof_dyn_type        = caml_named_value("sizeof_dyn_type");
  ocaml_shape_rank             = caml_named_value("shape_rank");
  ocaml_compact                = caml_named_value("c_compact");
#else
  ocaml_compiler_init          = caml_named_value("compiler_init");
  ocaml_gen_module_template    = caml_named_value("debug_gen_module_template");
  ocaml_get_function_template  =
    caml_named_value("debug_get_function_template");
  ocaml_run_template           = caml_named_value("debug_run_template");
  ocaml_ktypenum_to_ocaml_type = caml_named_value("ktypenum_to_ocaml_type");
  ocaml_dyn_type_to_ktypenum   = caml_named_value("dyn_type_to_ktypenum");
  ocaml_sizeof_dyn_type        = caml_named_value("sizeof_dyn_type");
  ocaml_shape_rank             = caml_named_value("shape_rank");
  ocaml_compact                = caml_named_value("c_compact");
#endif

  // Initialize templates storage - we need to keep pointers around so that
  // OCaml won't garbage collect stuff.;
  module_templates   = (value*)malloc(sizeof(value) * INITIAL_VECTOR_SIZE);
  function_templates = (value*)malloc(sizeof(value) * INITIAL_VECTOR_SIZE);
  sizeof_module_templates = sizeof_function_templates = INITIAL_VECTOR_SIZE;
  num_module_templates = num_function_templates = 0;

  // Initialize the OCaml runtime and CUDA.
  caml_callback(*ocaml_compiler_init, Val_unit);

  CAMLreturn0;
}

/*
 * Generates a template for the functions in a module.
 */
K gen_module_template(K functions) {
  CAMLparam0();
  CAMLlocal3(func_list1, func_list2, func_desc);

  // TODO: Really don't like this here, but only works if it is.
  init_dt();

  // Fill the OCaml block with the appropriate function info.
  int i;
  K *k_func_desc;
  int num_funcs = functions->n;
  if (num_funcs > 0) {
    // Extract the functions.
    for (i = num_funcs - 1; i >= 0; --i) {
      // Create the function descriptor and its list entry.
      k_func_desc = kK(kK(functions)[i]);
      func_desc   = caml_alloc_tuple(4);
      Store_field(func_desc, 0, k_string_to_ocaml(k_func_desc[0]));
      Store_field(func_desc, 1, gen_build_args_list(k_func_desc[1]));
      Store_field(func_desc, 2, gen_build_args_list(k_func_desc[2]));
      Store_field(func_desc, 3, k_string_to_ocaml(k_func_desc[3]));

      func_list2 = caml_alloc_tuple(2);
      Store_field(func_list2, 0, func_desc);
      if (i == num_funcs - 1) {
        // This is the first func we've seen/last func in list - place []
        Store_field(func_list2, 1, Val_int(0));
      } else {
        Store_field(func_list2, 1, func_list1);
      }
      func_list1 = func_list2;
    }
  } else {
    // Build an empty list - this probably shouldn't happen.
    func_list1 = Val_int(0);
  }

  // Call OCaml to get the module descriptor.
  if (num_module_templates == sizeof_module_templates) {
    // Extend vector
    value *new_vector = (value*)malloc(sizeof(value) * num_module_templates*2);
    for (i = 0; i < num_module_templates; ++i) {
      new_vector[i] = module_templates[i];
      caml_register_global_root(&new_vector[i]);
      caml_remove_global_root(&module_templates[i]);
    }
    sizeof_module_templates *= 2;
    free(module_templates);
    module_templates = new_vector;
  }
  caml_register_global_root(&module_templates[num_module_templates]);
  module_templates[num_module_templates] =
    caml_callback(*ocaml_gen_module_template, func_list1);

  // Return the result (as an index into the global vector) back to Q.
  K module_template = ki((I)num_module_templates);
  num_module_templates++;
  CAMLreturnT(K, module_template);
}

/*
 * Extracts a function template for a function in the given module.
 */
K get_function_template(K mod_temp, K func_name) {
  CAMLparam0();
  CAMLlocal3(camlmod_temp, camlfunc_name, camlfunc_template);

  camlmod_temp = module_templates[mod_temp->i];
  camlfunc_name = k_string_to_ocaml(func_name);

  if (num_function_templates == sizeof_function_templates) {
    // Extend vector
    value *new_vector = (value*)malloc(sizeof(value)*num_function_templates*2);
    int i;
    for (i = 0; i < num_function_templates; ++i) {
      new_vector[i] = function_templates[i];
      caml_register_global_root(&new_vector[i]);
      caml_remove_global_root(&function_templates[i]);
    }
    sizeof_function_templates *= 2;
    free(function_templates);
    function_templates = new_vector;
  }
  caml_register_global_root(&function_templates[num_function_templates]);
  function_templates[num_function_templates] = caml_callback2(
    *ocaml_get_function_template, camlmod_temp, camlfunc_name);
  K t = ki((I)num_function_templates);
  num_function_templates++;
  CAMLreturnT(K, t);
}

/**
 * Runs the given function with the given globals and args.
 *
 * Returns some particular type when we decide for whatever reason we don't
 * want to run the function in our framework to allow Q to do it for us.
 */
K run_template(K t, K args, K globals) {
  CAMLparam0();
  CAMLlocal4(ocaml_ret, ocaml_template, ocaml_globals, ocaml_args);

  // Need to create OCaml versions of the arguments
  ocaml_template = function_templates[t->i];
  ocaml_globals  = args_list_to_ocaml_list(globals);
  ocaml_args     = args_list_to_ocaml_list(args);

  inspect_block(ocaml_args);

  // Now actually call the OCaml function to build and run the CUDA function
  ocaml_ret = caml_callback3(
    *ocaml_run_template, ocaml_template, ocaml_globals, ocaml_args);

  // Have to unpack and repack the OCaml return value for Q
  K q_status, q_retval, q_ret;
  if (Is_long(ocaml_ret)) {
    q_status = ki((I)1);
    q_ret = knk(1, q_status);
  } else if (Tag_val(ocaml_ret) == Success) {
    inspect_block(ocaml_ret);
    q_status = ki((I)0);
    int *data = (int*)Int64_val(Field(ocaml_ret, 0));
    int len = Int_val(Field(ocaml_ret, 1)) / sizeof(int);
    int i;
    q_retval = hostval_to_qval(ocaml_ret);
    q_ret = knk(2, q_status, q_retval);
  } else if (Tag_val(ocaml_ret) == Error) {
    q_status = ki((I)2);
    q_retval = kp(String_val(Field(ocaml_ret, 0)));
    q_ret = knk(2, q_status, q_retval);
  } else {
    printf("Unexpected return value from ocaml_run_template. Aborting.\n");
    exit(-1);
  }

  CAMLreturnT(K, q_ret);
}

/**
 * Builds an args list for the gen_module_template function.
 */
value gen_build_args_list(K args) {
  CAMLparam0();
  CAMLlocal3(arg_name, arg1, arg2);

  int num_args, i;

  // Extract the args.
  num_args = args->n;
  if (num_args > 0) {
    //Extract args
    for (i = num_args - 2; i >= 0; --i) {
      if (args->t == KC) {
        // All the args were single-element, so we have just a single
        // list of chars (basically they got collapsed into a string by Q).
        // Thus we pull them all out one at a time ourselves.
        arg_name = caml_alloc_string(1);
        memcpy(String_val(arg_name), &kC(args)[i], 1);
      } else if (args->t == 0) {
        // This is a list of char lists - the normal thing we would expect.
        arg_name = k_string_to_ocaml(kK(args)[i]);
      } else if (args->t == -KC) {
        // There was only one argument, and thus we have a char not a list.
        arg_name = caml_alloc_string(1);
        memcpy(String_val(arg_name), &args->g, 1);
      } else {
        // Unexpected type
        printf("Unexpected type in args list (%d). Exiting.\n", args->t);
        exit(-1);
      }
      arg2 = caml_alloc_tuple(2);
      Store_field(arg2, 0, arg_name);

      if (i == num_args - 2) {
        // First arg we've seen - store [] at end of list
        Store_field(arg2, 1, Val_int(0));
      } else {
        Store_field(arg2, 1, arg1);
      }
      arg1 = arg2;
    }
  } else {
    // Have to build an empty list
    arg1 = Val_int(0);
  }

  CAMLreturn(arg1);
}

/**
 * Takes a list of args (or globals) and returns the OCaml version of them.
 */
value args_list_to_ocaml_list(K args) {
  CAMLparam0();
  CAMLlocal2(ocaml_arg1, ocaml_arg2);

 // printf("in args_list_to_ocaml_list\n");

  // Loop over the args, building an OCaml list of host_vals
  int num_args = args->n;
  int i;
  if (num_args > 0) {
    if (args->t == 0) {
      // Mixed list
      for (i = num_args - 2; i >= 0; --i) {
        ocaml_arg2 = caml_alloc_tuple(2);
        Store_field(ocaml_arg2, 0, k_var_to_ocaml(kK(args)[i]));
        if (i == num_args - 2) {
          Store_field(ocaml_arg2, 1, Val_int(0));
        } else {
          Store_field(ocaml_arg2, 1, ocaml_arg1);
        }
        ocaml_arg1 = ocaml_arg2;
      }
    } else {
      // List of all the same type of scalar
     // printf("list of all same type\n");
      for (i = num_args - 2; i >= 0; --i) {
        ocaml_arg2 = caml_alloc_tuple(2);
        switch(args->t) {
        case KI:
          Store_field(ocaml_arg2, 0, k_var_to_ocaml(ki(kI(args)[i])));
          break;
        case KJ:
          Store_field(ocaml_arg2, 0, k_var_to_ocaml(kj(kJ(args)[i])));
          break;
        case KE:
          Store_field(ocaml_arg2, 0, k_var_to_ocaml(ke(kE(args)[i])));
          break;
        case KF:
          Store_field(ocaml_arg2, 0, k_var_to_ocaml(kf(kF(args)[i])));
          break;
        case KC:
          Store_field(ocaml_arg2, 0, k_var_to_ocaml(kc(kC(args)[i])));
          break;
        default:
          // TODO: Revisit this...
          printf("Unsupported argument to run_template. Aborting.\n");
          exit(-1);
        }
        if (i == num_args - 2) {
          Store_field(ocaml_arg2, 1, Val_int(0));
        } else {
          Store_field(ocaml_arg2, 1, ocaml_arg1);
        }
        ocaml_arg1 = ocaml_arg2;
      }
    }
  } else {
    ocaml_arg1 = Val_int(0);
  }

  CAMLreturn(ocaml_arg1);
}

/**
 * Takes a K string/char and creates an OCaml string from it.  Handles the case
 * where the K string is actually a single char rather than a 1-element char
 * list.
 */
value k_string_to_ocaml(K kstr) {
  CAMLparam0();
  CAMLlocal1(ocaml_str);

  int len;

  if (kstr->t == -KC) {
    // Single-char string is treated as a character!
    ocaml_str = caml_alloc_string(1);
    memcpy(String_val(ocaml_str), &kstr->g, 1);
  } else if (kstr->t == KC) {
    len = kstr->n;
    ocaml_str = caml_alloc_string(len);
    memcpy(String_val(ocaml_str), (const void*)kC(kstr), len);
  } else {
    printf("Attempting to make a string from an incompatible K type."
           "Aborting.\n");
    exit(-1);
  }

  CAMLreturn(ocaml_str);
}

/*
 * Takes a K variable and constructs an OCaml host_val record for it, which
 * contains:
 *  - a pointer to the variable's data, flattened into a contiguous block
 *  - the number of bytes in total of the variable
 *  - the variable's dyn_type
 *  - the variable's shape
 */
value k_var_to_ocaml(K kvar) {
  CAMLparam0();
  CAMLlocal2(ocaml_var, ocaml_dyn_type);

  int num_bytes, shape_len;
  int *shape;
  char *flattened;
  get_flattened_version(kvar, &num_bytes, &ocaml_dyn_type, &shape, &shape_len,
                        &flattened);
  ocaml_var = build_ocaml_hostval(num_bytes, ocaml_dyn_type, shape, shape_len,
                                  flattened);
  printf("bult ocaml var\n");
  CAMLreturn(ocaml_var);
}

void get_flattened_version(K kvar, int *num_bytes, value *ocaml_dyn_type,
                           int **shape, int *shape_len, char **flattened) {
  // Construct the dyn_type and get the total number of bytes in the kvar.
  // We start the shape vec to be of length 4 cuz it'll probably not ever get
  // bigger than that.
  *num_bytes = 0;
  *shape_len = 0;
  int shape_array_len = 4;
  *shape = (int*)malloc(shape_array_len * sizeof(int));
  get_kvar_representation(kvar, num_bytes, ocaml_dyn_type,
                          shape, shape_len, &shape_array_len);

  printf("called get_kvar from get_flattened\n");

  // Allocate a linear array of the correct number of bytes to store the
  // flattened representation.
  *flattened = (char*)malloc(*num_bytes);
  int cur_idx = 0;
  flatten_kvar(kvar, *flattened, &cur_idx);
}

// Recursively counts the number of bytes, builds a dyn_type for the var, and
// creates the shape vector.
//
// For now, I'm just assuming that hierarchical types are uniform - that all
// elements at a given level are of the same type and length.  This will have
// to be revisited later.
//
// TODO: For now, I'm _only_ dealing with scalars and linear vectors.  Will
//       have to deal with this better later.
// TODO: Add error checking to make sure the array is uniform
void get_kvar_representation(K kvar, int *num_bytes, value *ocaml_dyn_type,
                             int **shape, int *cur_shape_len,
                             int *shape_array_len) {
  CAMLparam0();

  int i;
  if (kvar->t < 0) {
    // Scalar - easy.  We don't update the shape here - the 1 is implicit.
    *ocaml_dyn_type =
      caml_callback(*ocaml_ktypenum_to_ocaml_type, Val_int(-kvar->t));
    *num_bytes = ktype_num_bytes(kvar->t);
  } else if (kvar->t == 0) {
    // A list of non-scalars. Have to do recursion. Again, for now assuming that
    // the list has uniform children, so we'll only recurse into the first one.

    // Allocate a value to store the dyn_type of the children
    *ocaml_dyn_type = caml_alloc(1, VecT);

    // Update the shape vector, extending it if necessary
    if (*cur_shape_len == *shape_array_len) {
      int *new_shape = (int*)malloc(sizeof(int) * 2 * (*shape_array_len));
      for (i = 0; i < *shape_array_len; ++i) {
        new_shape[i] = (*shape)[i];
      }
      free(*shape);
      *shape = new_shape;
    }
    (*shape)[*cur_shape_len] = kvar->n;
    *cur_shape_len = (*cur_shape_len) + 1;

    // Recurse into the type.
    get_kvar_representation(kK(kvar)[0], num_bytes,
                            &Field(*ocaml_dyn_type, 0), shape, cur_shape_len,
                            shape_array_len);

    // Update the number of bytes to be the current amount * kvar->n
    *num_bytes = (*num_bytes) * kvar->n;
  } else {
    // TODO: Assuming it's a linear vector for now
    //
    // As an optimization, I won't recurse into the scalar type here, just take
    // care of biz right now.

    // Allocate a value to store the dyn_type of the children
    *ocaml_dyn_type = caml_alloc(1, VecT);
    Store_field(*ocaml_dyn_type, 0,
                caml_callback(*ocaml_ktypenum_to_ocaml_type,
                              Val_int(kvar->t)));

    // Update the shape vector, extending it if necessary
    if (*cur_shape_len == *shape_array_len) {
      int *new_shape = (int*)malloc(sizeof(int) * 2 * (*shape_array_len));
      for (i = 0; i < *shape_array_len; ++i) {
        new_shape[i] = (*shape)[i];
      }
      free(*shape);
      *shape = new_shape;
    }
    (*shape)[*cur_shape_len] = kvar->n;
    *cur_shape_len = (*cur_shape_len) + 1;

    *num_bytes = ktype_num_bytes(-kvar->t) * kvar->n;
  }

  CAMLreturn0;
}

/*
 * Takes a kvar and a block of memory pre-allocated to hold a flattened version
 * of the kvar's data and populates the array.
 *
 * The flattened version in effect amounts to a depth-first-search walk of the
 * kvar.
 */
void flatten_kvar(K kvar, char *flattened, int *cur_idx) {
  int num_bytes = 0;
  int i;

  if (kvar->t == 0) {
    // TODO: As always, assuming uniformity.
    for (i = 0; i < kvar->n; ++i) {
      flatten_kvar(kK(kvar)[i], flattened, cur_idx);
    }
  } else if (kvar->t < 0) {
    // Scalar - copy the data right in.  This should really only be called when
    // the whole variable is scalar.
    num_bytes = ktype_num_bytes(kvar->t);
    switch(-kvar->t) {
    case KI:
      memcpy(flattened + *cur_idx, &kvar->i, sizeof(I));
      break;
    case KJ:
      memcpy(flattened + *cur_idx, &kvar->j, sizeof(J));
      break;
    case KE:
      memcpy(flattened + *cur_idx, &kvar->e, sizeof(E));
      break;
    case KF:
      memcpy(flattened + *cur_idx, &kvar->f, sizeof(F));
      break;
    default:
      printf("Unsupported K Type to flatten.\n");
      exit(-1);
    }
    *cur_idx = (*cur_idx) + num_bytes;
  } else {
    // TODO: Assuming dicts, tables, and errors don't happen.
    num_bytes = ktype_num_bytes(-kvar->t) * kvar->n;
    memcpy(flattened + *cur_idx, kG(kvar), num_bytes);
    *cur_idx = (*cur_idx) + num_bytes;
  }
}

int ktype_num_bytes(int ktype) {
  switch(ktype) {
  case -KB:
    return 1;
  case -KG:
    return 1;
  case -KH:
    return 2;
  case -KI:
    return 4;
  case -KJ:
    return 8;
  case -KE:
    return 4;
  case -KF:
    return 8;
  case -KC:
    return 1;
  case -KS:
    return 8; // Assuming 64-bits here!
  case -KM:
    return 4;
  case -KD:
    return 4;
  case -KZ:
    return 8;
  case -KU:
  case -KV:
  case -KT:
    return 4;
  case XT:
  case XD:
    // For now tables and dicts aren't handled
  default:
    return -1;
  }

  return -1;
}

K hostval_to_qval(value hostval) {
  CAMLparam1(hostval);
  CAMLlocal1(ocaml_dyn_type);

  K ret;
  int i;
  char *data     = (char*)Nativeint_val(Field(hostval, 0));
  int num_bytes  = Int_val(Field(hostval, 1));
  ocaml_dyn_type = Field(hostval, 2);
  int *shape     = (int*)Data_bigarray_val(Field(hostval, 3));
  int shape_len  = Int_val(caml_callback(*ocaml_shape_rank,
                                          Field(hostval, 3)));
  int *idxes     = (int*)malloc(sizeof(int)*shape_len);
  for (i = 0; i < shape_len; ++i) {
    idxes[i] = 0;
  }

  //printf("about to build q type\n");
  int ktypenum = -1 * abs(Int_val(caml_callback(*ocaml_dyn_type_to_ktypenum,
                                                ocaml_dyn_type)));
  ret = build_q_type(data, num_bytes, ktypenum,
                     shape, shape_len, 0, idxes);

  // TODO: Do we want to free the data now? I guess so.
  //free(data);
  free(idxes);

  CAMLreturnT(K, ret);
}

/*
 * Recursively builds the K struct.  At the moment, we have to alloc and re-
 * copy the stuff into Q, because Q's objects are allocated with headers
 * attached to their memory payload (which our flattened representation doesn't
 * have).
 */
K build_q_type(char *data, int num_bytes, int ktypenum,
               int *shape, int shape_len, int shape_idx, int *idxes) {
  K ret;
  int i;

  int sizeof_element = 0;
  if (shape_len == 0) {
    // Scalar
    switch(ktypenum) {
    case KB:
      ret = kb((int)data[0]);
      break;
    case KI:
      // int
      ret = ki(((int*)data)[0]);
      break;
    case KJ:
      // long
      ret = kj(((long*)data)[0]);
      break;
    case KE:
      ret = ke(((float*)data)[0]);
      break;
    case KF:
      // float
      ret = kf(((double*)data)[0]);
      break;
    case KS:
      // symbol
      ret = ks(ss(data));
      break;

    /** Default: Error **/
    default:
      printf("Unsupported Q type coming back from dyn_type_to_ktypenum\n");
      exit(-1);
    }
  } else if (shape_idx == shape_len - 1) {
    // Last level of lists, just build
    if (sizeof_element == 0) {
      sizeof_element = ktype_num_bytes(ktypenum);
    }
    ret = ktn(-ktypenum, shape[shape_idx]);
    int idx = 0, factor = sizeof_element;
    for (i = shape_idx; i > 0; --i) {
      factor *= shape[i];
      idx += idxes[i - 1] * factor;
    }
    memcpy((void*)kG(ret), data + idx, shape[shape_idx]*sizeof_element);
  } else {
    // Non-scalar
    // TODO: only vecs now!!
    // List of lists, have to recurse
    ret = ktn(0, shape[shape_idx]);
    for (i = 0; i < shape[shape_idx]; ++i) {
      idxes[shape_idx] = i;
      kK(ret)[i] = build_q_type(data, num_bytes, ktypenum,
                                shape, shape_len, shape_idx + 1, idxes);
    }
  }

  return ret;
}

/**
 * Outputs the given K variables in a binary format to the specified output
 * file.
 */
K dump_variables(K filename, K vars) {
  CAMLparam0();
  CAMLlocal2(ocaml_dyn_type, ocaml_scalar_type);

  // TODO: remove
  init_dt();

  char *cname;
  if (filename->t == -KC) {
    cname = (char*)malloc(2);
    memcpy(cname, &filename->g, 1);
    cname[1] = '\0';
  } else {
    cname = (char*)malloc(filename->n + 1);
    memcpy(cname, (const void*)kC(filename), filename->n);
    cname[filename->n] = '\0';
  }

  // Open the file
  FILE *ofile = fopen(cname, "w");

  int num_vars = vars->n;

  // Write file header
  fwrite(&num_vars, sizeof(int), 1, ofile);
  
  // Write each var's record
  K *kvar;
  int i, len, type, num_bytes, shape_len;
  int *shape;
  char *flattened;
  for (i = 0; i < num_vars; ++i) {
    // Write the variable's name
    kvar = kK(kK(vars)[i]);
    if (kvar[0]->t == -KC) {
      len = 1;
      fwrite(&len, sizeof(int), 1, ofile);
      fwrite(&kvar[0]->g, sizeof(char), 1, ofile);
    } else {
      len = kvar[0]->n;
      fwrite(&len, sizeof(int), 1, ofile);
      fwrite((const void*)kC(kvar[0]), 1, len, ofile);
    }

    // Get the variable's scalar type
    K tmp = kvar[1];
    while (tmp->t == 0) {
      tmp = kK(tmp)[0];
    }
    if (tmp->t < 0) {
      type = -tmp->t;
    } else {
      type = tmp->t;
    }
    ocaml_scalar_type =
      caml_callback(*ocaml_ktypenum_to_ocaml_type, Val_int(type));

    // Get the variable's flattened representation
    get_flattened_version(kvar[1], &num_bytes, &ocaml_dyn_type,
                          &shape, &shape_len, &flattened);
    // Write it to the file
    fwrite(&shape_len, sizeof(int), 1, ofile);
    fwrite(shape, sizeof(int), shape_len, ofile);
    fwrite(&ocaml_scalar_type, sizeof(value), 1, ofile);
    fwrite(&num_bytes, sizeof(int), 1, ofile);
    fwrite(flattened, sizeof(char), num_bytes, ofile);

    // Clean up memory
    free(shape);
    free(flattened);
  }

  fclose(ofile);
  free(cname);

  CAMLreturnT(K, ki((I)1));
}

/**
 * I took the following three functions from this website:
 *
 * http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora115.html
 *
 * so we might want to remove them later.
 */
void margin (int n)
  { while (n-- > 0) printf(".");  return; }

void print_block (value v,int m) 
{
  int size, i;
  margin(m);
  if (Is_long(v)) 
    { printf("immediate value (%ld)\n", Long_val(v));  return; };
  printf ("memory block: size=%d  -  ", size=Wosize_val(v));
  switch (Tag_val(v))
   {
    case Closure_tag : 
        printf("closure with %d free variables\n", size-1);
        margin(m+4); printf("code pointer: %p\n",Code_val(v)) ;
        for (i=1;i<size;i++)  print_block(Field(v,i), m+4);
        break;
    case String_tag :
        printf("string: %s (%s)\n", String_val(v),(char *) v);  
        break;
    case Double_tag:  
        printf("float: %g\n", Double_val(v));
        break;
    case Double_array_tag : 
        printf ("float array: "); 
        for (i=0;i<size/Double_wosize;i++)  printf("  %g", Double_field(v,i));
        printf("\n");
        break;
    case Abstract_tag : printf("abstract type\n"); break;
    default:  
        if (Tag_val(v)>=No_scan_tag) { printf("unknown tag\n"); break; }; 
        printf("structured block (tag=%d):\n",Tag_val(v));
        for (i=0;i<size;i++)  print_block(Field(v,i),m+4);
   }
  return ;
}

value inspect_block (value v)
  { print_block(v,4); fflush(stdout); return Val_unit; }
