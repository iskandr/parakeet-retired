/*
 *  dt.h
 *
 * Interface between the OCaml and Q runtimes.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2010.
 */

#include <caml/mlvalues.h>
#include "k.h"
#include "../OCAMLInterface/variants.h"


/** Main interface functions between preprocessed Q and OCaml. **/
K gen_module_template(K functions);
K get_function_template(K mod_temp, K func_name);
K run_template(K t, K args, K globals);
K dump_variables(K filename, K vars);

/** Helper interface functions between C and OCaml. **/
void init_dt();
value k_var_to_ocaml(K kvar);
value ktypenum_to_ocaml_type(int64_t ktypenum);
K hostval_to_qval(value hostval);
void get_kvar_representation(K kvar, int *num_bytes, value *ocaml_dyn_type,
                             int **shape, int *cur_shape_len,
                             int *shape_array_len);
void flatten_kvar(K kvar, char *flattened, int *cur_idx);
void get_flattened_version(K kvar, int *num_bytes, value *ocaml_dyn_type,
                           int **shape, int *shape_len, char **flattened);
int ktype_num_bytes(int ktype);
K build_q_type(char *data, int num_bytes, int ktypenum,
               int *shape, int shape_len, int shape_idx, int *idxes);
value gen_build_args_list(K args);
value args_list_to_ocaml_list(K args);
value k_string_to_ocaml(K kstr);

/**
 * I took the following three functions from this website:
 *
 * http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora115.html
 *
 * so we might want to remove them later.
 */
void margin (int n);
void print_block (value v,int m);
value inspect_block (value v);
