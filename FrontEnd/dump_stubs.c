/*
 * (c) 2010 Eric Hielscher, Alex Rubinsteyn
 *
 */

#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

#include <stdint.h>
#include <stdio.h>
#include <string.h> 

#include "ocaml_functions.h"
#include "../OCAMLInterface/variants.h"

// Takes a dump data file and creates an OCaml list of all the variables in
// it.

value read_dump_file(value filename) {
  CAMLparam1(filename);
  CAMLlocal5(ocaml_var, ocaml_list_el1, ocaml_list_el2,
             ocaml_dyn_type, ocaml_dyn_aux);
  CAMLlocal2(ocaml_name, ocaml_hostval);

  printf("opening %s\n", String_val(filename));
  FILE *ifile = fopen(String_val(filename), "r");
  if (ifile == NULL) {
    ocaml_var = Val_int(0);
    CAMLreturn(ocaml_var);
  }

  int num_vars = 0;
  fread(&num_vars, sizeof(int), 1, ifile);
  if (num_vars == 0) {
    ocaml_var = Val_int(0);
    CAMLreturn(ocaml_var);
  }

  printf("parsing vars\n");
  ocaml_list_el2 = caml_alloc_tuple(2);
  int i, j;
  int name_len, shape_len, type, num_bytes;
  int *shape;
  char *name, *data;
  for (i = 0; i < num_vars; ++i) {
    // Allocate the variable tuple of (name, hostval)
    ocaml_var = caml_alloc_tuple(2);

    // Build the name
    fread(&name_len, sizeof(int), 1, ifile);
    ocaml_name = caml_alloc_string(name_len);
    fread(String_val(ocaml_name), sizeof(char), name_len, ifile);
    Store_field(ocaml_var, 0, ocaml_name);
    printf("got var name: %s\n", String_val(ocaml_name));

    // Build the shape
    fread(&shape_len, sizeof(int), 1, ifile);
    shape = (int*)malloc(shape_len * sizeof(int));
    fread(shape, sizeof(int), shape_len, ifile);
    printf("got shape of len %d\n", shape_len);

    // Build the DynType
    fread(&ocaml_dyn_type, sizeof(value), 1, ifile);
    for (j = 0; j < shape_len; ++j) {
      ocaml_dyn_aux = ocaml_dyn_type;
      ocaml_dyn_type = caml_alloc(1, VecT);
      Store_field(ocaml_dyn_type, 0, ocaml_dyn_aux);
    }
    printf("built dyn_type\n");

    // Get the num_bytes
    fread(&num_bytes, sizeof(int), 1, ifile);

    // Get the payload
    data = (char*)malloc(num_bytes);
    fread(data, 1, num_bytes, ifile);
    printf("got payload of %d bytes\n", num_bytes);

    // Build the HostVal
    ocaml_hostval = build_ocaml_hostval(num_bytes, ocaml_dyn_type,
                                        shape, shape_len, data);
    printf("built hostval from inputs\n");
    Store_field(ocaml_var, 1, ocaml_hostval);
    
    // Insert the var into the list
    if (i == 0) {
      Store_field(ocaml_list_el2, 1, Val_int(0));
    } else {
      Store_field(ocaml_list_el2, 1, ocaml_list_el1);
    }
    Store_field(ocaml_list_el2, 0, ocaml_var);
    if (i < num_vars - 1) {
      ocaml_list_el1 = ocaml_list_el2;
      ocaml_list_el2 = caml_alloc_tuple(2);
    }
  }

  fclose(ifile);

  CAMLreturn(ocaml_list_el2);
}

