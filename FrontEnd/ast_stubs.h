/*
 *  ast_stubs.c
 *
 *  Functions for front ends to create Parakeet ASTs.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2011.
 */

#ifndef _AST_STUBS_H_
#define _AST_STUBS_H_

#include "prim_variants.h"

/**
 * We'll eventually want to include source info alongside the tree nodes so as
 * to facilitate debugging messages.  Passing pointers to these structs is thus
 * optional to each of the mk_X functions.
 */
typedef struct source_info {
  char *filename;
  int   line;
  int   col;
} source_info_t;

typedef void* paranode;

/** Initialization function - call before using any creation functions **/
void ast_init(void);

/** paranode creation functions **/
paranode mk_lam(char **args, int num_args, paranode body,
                source_info_t *src_info);
  
paranode mk_var(char *str, source_info_t *src_info);

paranode mk_scalar_op(scalar_op_t op, source_info_t *src_info);
paranode mk_array_op(array_op_t op, source_info_t *src_info);
paranode mk_adverb(adverb_t op, source_info_t *src_info);
paranode mk_impure_op(impure_op_t op, source_info_t *src_info);
paranode mk_q_op(q_op_t op, source_info_t *src_info);

paranode mk_int32_paranode(int32_t i, source_info_t *src_info);
paranode mk_int64_paranode(int64_t l, source_info_t *src_info);
paranode mk_float_paranode(float f, source_info_t *src_info);
paranode mk_double_paranode(double d, source_info_t *src_info);

paranode mk_str(char *str, source_info_t *src_info);

paranode mk_sym(char *sym, source_info_t *src_info);

/** Accepts an array of args **/
paranode mk_app(paranode fun, paranode *args, int num_args,
                source_info_t *src_info);

paranode mk_arr(paranode nodes, source_info_t *src_info);

paranode mk_if(paranode cond_node, paranode true_node, paranode false_node,
               source_info_t *src_info);

paranode mk_def(char *name, paranode rhs, source_info_t *src_info);

paranode mk_setidx(char *arr_name, paranode idxs, paranode rhs,
                   source_info_t *src_info);

paranode mk_block(paranode stmts, source_info_t *src_info);

paranode mk_whileloop(paranode test, paranode body,
                      source_info_t *src_info);

paranode mk_countloop(paranode count, paranode body,
                      source_info_t *src_info);

paranode mk_void(source_info_t *src_info);

#endif