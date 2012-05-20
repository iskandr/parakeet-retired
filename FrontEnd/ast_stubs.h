/*
 *  ast_stubs.c
 *
 *  Functions for front ends to create Parakeet ASTs.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2011.
 */

#ifndef _AST_STUBS_H_
#define _AST_STUBS_H_

#include <caml/mlvalues.h>
#include <stdint.h>

typedef struct {
  value v;
} paranode_t;

typedef paranode_t* paranode;

typedef struct source_info {
  char *filename;
  int   line;
  int   col;
} source_info_t;

/** Initialization function - call before using any creation functions **/
void ast_init(void);

/** paranode creation functions **/
paranode mk_lambda(char **args, int num_args, paranode body,
                source_info_t *src_info);
  
paranode mk_var(char *str, source_info_t *src_info);

paranode mk_bool_paranode(int, source_info_t*);
paranode mk_int32_paranode(int32_t i, source_info_t *src_info);
paranode mk_int64_paranode(int64_t l, source_info_t *src_info);
paranode mk_float_paranode(float f, source_info_t *src_info);
paranode mk_double_paranode(double d, source_info_t *src_info);

paranode mk_str(char *str, source_info_t *src_info);

/** Accepts an array of args **/
paranode mk_call(paranode fun, paranode *args, int num_args,
                source_info_t *src_info);

paranode mk_return(paranode* args, int num_args, source_info_t *src_info);

paranode mk_array(paranode *elts, int num_stmts, source_info_t *src_info);

paranode mk_if(paranode cond_node, paranode true_node, paranode false_node,
               source_info_t *src_info);

paranode mk_assign(paranode* ids, int num_ids, paranode rhs,
                   source_info_t *src_info);

paranode mk_block(paranode *stmts, int num_stmts, source_info_t *src_info);

paranode mk_whileloop(paranode test, paranode body, source_info_t *src_info);

paranode mk_countloop(paranode count, paranode body, source_info_t *src_info);

paranode mk_none(source_info_t *src_info);

paranode get_prim(char* prim_name);

void print_ast_node(paranode n);

CAMLprim value mk_val_list(paranode *vals, int num_vals);

#endif
