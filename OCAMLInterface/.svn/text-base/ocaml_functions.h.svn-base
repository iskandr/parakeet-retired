/*
 *  ocaml_functions.h
 *
 * Interface between the OCaml runtime and our internal C-based runtime.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2010.
 */

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

value build_ocaml_hostval(int num_bytes, value ocaml_dyn_type,
                          int *shape, int shape_len, char *flattened);

