/*
 *  ast_stubs.c
 *
 *  Functions for front ends to create Parakeet ASTs.
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

#include "ast_variants.h"
#include "variants.h"

#include "ast_stubs.h"

/** Private members **/
value *ocaml_mk_ast_info = NULL;
value *ocaml_get_prim = NULL;
value *ocaml_print_ast_node = NULL; 

static int ast_inited = 0;
static CAMLprim value mk_src_info(source_info_t *src_info);
static paranode mk_num(value num, source_info_t *src_info);
static CAMLprim value get_value_and_remove_root(paranode p);
static CAMLprim value mk_val_list(paranode *vals, int num_vals);
static paranode mk_root(value v);
static paranode mk_node(value exp, source_info_t *src_info);

/** Public interface **/

void ast_init(void) {
  if (ast_inited) return;

  ast_inited = 1;

  ocaml_mk_ast_info    = caml_named_value("mk_ast_info");
  ocaml_print_ast_node = caml_named_value("print_ast_node"); 
  ocaml_get_prim       = caml_named_value("get_prim");
}

paranode mk_lam(char **args, int num_args, paranode body,
                source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal5(lam, ocaml_str, arg1, arg2, node);
  CAMLlocal1(val_body);

  val_body = get_value_and_remove_root(body);

  int len, i;

  // Build the args list
  if (num_args > 0) {
    // Create the tail of the list
    len  = strlen(args[num_args - 1]);
    ocaml_str = caml_alloc_string(len);
    memcpy(String_val(ocaml_str), args[num_args - 1], len);
    arg1 = caml_alloc_tuple(2);
    Store_field(arg1, 0, ocaml_str);
    Store_field(arg1, 1, Val_int(0));

    // Extract each arg and add it to the OCaml list
    for (i = num_args - 2; i >= 0; --i) {
      len = strlen(args[i]);
      ocaml_str = caml_alloc_string(len);
      memcpy(String_val(ocaml_str), args[i], len);
      arg2 = caml_alloc_tuple(2);
      Store_field(arg2, 0, ocaml_str);
      Store_field(arg2, 1, arg1);
      arg1 = arg2;
    }
  } else {
    arg1 = Val_int(0);
  }

  // Build the lambda expression
  lam = caml_alloc(2, Exp_Lam);
  Store_field(lam, 0, arg1);
  Store_field(lam, 1, val_body);

  // Build the node and return
  CAMLreturnT(paranode, mk_node(lam, src_info));
}

paranode mk_var(char *str, source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal2(ocaml_str, var);

  // copy over the string
  int len = strlen(str);
  ocaml_str = caml_alloc_string(len);
  memcpy(String_val(ocaml_str), str, len);
  printf("in mk_var\n");
  fflush(stdout);

  // build the var expression
  var = caml_alloc(1, Exp_Var);
  Store_field(var, 0, ocaml_str);

  // build the node and return
  CAMLreturnT(paranode, mk_node(var, src_info));
}

paranode mk_bool_paranode(int b, source_info_t *src_info) {
   CAMLparam0();
   CAMLlocal1(val);

   val = caml_alloc(1, PARNUM_BOOL);
   Store_field(val, 0, Val_int(b));
   CAMLreturnT(paranode, mk_num(val, src_info));
}

paranode mk_int32_paranode(int32_t i, source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal1(val);

  val = caml_alloc(1, PARNUM_INT32);
  Store_field(val, 0, caml_copy_int32(i));
  CAMLreturnT(paranode, mk_num(val, src_info));
}
  
paranode mk_int64_paranode(int64_t l, source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal1(val);

  val = caml_alloc(1, PARNUM_INT64);
  Store_field(val, 0, caml_copy_int64(l));

  CAMLreturnT(paranode, mk_num(val, src_info));
}

paranode mk_float_paranode(float f, source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal1(val);

  val = caml_alloc(1, PARNUM_FLOAT32);
  Store_field(val, 0, caml_copy_double((double)f));
  CAMLreturnT(paranode, mk_num(val, src_info));
}

paranode mk_double_paranode(double d, source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal1(val);

  val = caml_alloc(1, PARNUM_FLOAT64);
  Store_field(val, 0, caml_copy_double(d));
  CAMLreturnT(paranode, mk_num(val, src_info));
}

paranode mk_str(char *str, source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal2(ocaml_str, exp_str);

  int len = strlen(str);
  ocaml_str = caml_alloc_string(len);
  memcpy(String_val(ocaml_str), str, len);

  exp_str = caml_alloc(1, Exp_Str);
  Store_field(exp_str, 0, ocaml_str);

  CAMLreturnT(paranode, mk_node(exp_str, src_info));
}

paranode mk_app(paranode fun, paranode *args, int num_args,
                source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal3(val_fun, app, val_args);
  val_args = mk_val_list(args, num_args); 
  val_fun = get_value_and_remove_root(fun);
  app = caml_alloc(2, Exp_App);
  Store_field(app, 0, val_fun);
  Store_field(app, 1, val_args);

  CAMLreturnT(paranode, mk_node(app, src_info));
}

paranode mk_return(paranode* args, int num_args, source_info_t *src_info) { 
  CAMLparam0();
  CAMLlocal2(ret, ret_args);
  ret_args = mk_val_list(args, num_args);
  ret = caml_alloc(1, Exp_Return);
  Store_field(ret, 0, ret_args);
  CAMLreturnT(paranode, mk_node(ret, src_info));
}

paranode mk_array(paranode *elts, int num_elts, source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal2(arr, elt_list);
  elt_list = mk_val_list(elts, num_elts); 
  arr = caml_alloc(1, Exp_Arr);
  Store_field(arr, 0, elt_list);
  CAMLreturnT(paranode, mk_node(arr, src_info));
}

paranode mk_if(paranode cond_node, paranode true_node, paranode false_node,
               source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal4(val_cond, val_true, val_false, if_node);

  if_node = caml_alloc(3, Exp_If);

  val_cond  = get_value_and_remove_root(cond_node);
  Store_field(if_node, 0, val_cond);

  val_true  = get_value_and_remove_root(true_node);
  Store_field(if_node, 1, val_true);

  val_false = get_value_and_remove_root(false_node);
  Store_field(if_node, 2, val_false);

  CAMLreturnT(paranode, mk_node(if_node, src_info));
}

paranode mk_assign(paranode* lhs, int num_ids, paranode rhs,
                   source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal3(id_list, val_rhs, assignment);
  
  id_list = mk_val_list(lhs, num_ids);
  val_rhs = get_value_and_remove_root(rhs);

  assignment = caml_alloc(2, Exp_Assign);
  Store_field(assignment, 0, id_list);
  Store_field(assignment, 1, val_rhs);

  CAMLreturnT(paranode, mk_node(assignment, src_info));
}

paranode mk_block(paranode *stmts, int num_stmts, source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal2(block, stmt_list);
  stmt_list = mk_val_list(stmts, num_stmts); 
  block = caml_alloc(1, Exp_Block);
  Store_field(block, 0, stmt_list);
  CAMLreturnT(paranode, mk_node(block, src_info));
}

paranode mk_whileloop(paranode test, paranode body, source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal3(val_test, val_body, loop);

  val_test = get_value_and_remove_root(test);
  val_body = get_value_and_remove_root(body);

  loop = caml_alloc(1, Exp_WhileLoop);
  Store_field(loop, 0, val_test);
  Store_field(loop, 1, val_body);

  CAMLreturnT(paranode, mk_node(loop, src_info));
}

paranode mk_countloop(paranode count, paranode body, source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal3(val_count, val_body, loop);

  val_count = get_value_and_remove_root(count);
  val_body  = get_value_and_remove_root(body);

  loop = caml_alloc(1, Exp_CountLoop);
  Store_field(loop, 0, val_count);
  Store_field(loop, 1, val_body);

  CAMLreturnT(paranode, mk_node(loop, src_info));
}

paranode mk_void(source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal1(v);
  v = Val_int(Exp_Void);
  CAMLreturnT(paranode, mk_node(v, src_info));
}

paranode get_prim(char* prim_name) {
  CAMLparam0();
  CAMLlocal2(ocaml_str, prim);

  // copy over the string
  int len = strlen(prim_name);
  ocaml_str = caml_alloc_string(len);
  memcpy(String_val(ocaml_str), prim_name, len);

  // build the var expression
  prim = caml_callback(*ocaml_get_prim, ocaml_str);

  // build the node and return
  CAMLreturnT(paranode, mk_root(prim));
}

void print_ast_node(paranode n) { 
  CAMLparam0();
  CAMLlocal1(v);
  v = ((paranode_t*)n)->v;
  printf("in print_ast: %p\n", (paranode_t*)n);
  printf("n->v: %p\n", ((paranode_t*)n)->v);
  caml_callback(*ocaml_print_ast_node, v);
  CAMLreturn0;
}

/** Private functions **/

static paranode mk_num(value val, source_info_t *src_info) {
  CAMLparam1(val);
  CAMLlocal1(num);

  num = caml_alloc(1, Exp_Num);
  Store_field(num, 0, val);

  CAMLreturnT(paranode, mk_node(num, src_info));
}

static CAMLprim value mk_src_info(source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal3(ocaml_src_info, file, some_none);
  
  if (src_info) {
    if (src_info->filename) {
      int len = strlen(src_info->filename);
      file = caml_alloc_string(len);
      memcpy(String_val(file), src_info->filename, len);
      some_none = caml_alloc_tuple(1);
      Store_field(some_none, 0, file);
    } else {
      some_none = Val_int(0);
    }

    ocaml_src_info = caml_alloc_tuple(3);
    Store_field(ocaml_src_info, 0, some_none);
    Store_field(ocaml_src_info, 1, Val_int(src_info->line));
    Store_field(ocaml_src_info, 2, Val_int(src_info->col));
  } else {
    ocaml_src_info = caml_alloc_tuple(3);
    Store_field(ocaml_src_info, 0, Val_int(0));
    Store_field(ocaml_src_info, 1, Val_int(0));
    Store_field(ocaml_src_info, 2, Val_int(0));
  }

  CAMLreturn(ocaml_src_info);
}

static CAMLprim value get_value_and_remove_root(paranode node) {
  CAMLparam0();
  CAMLlocal1(val);
  paranode_t* p = (paranode_t*)node;
  val = p->v;
  caml_remove_global_root(&(p->v));
  free(p);
  CAMLreturn(val);
}

static CAMLprim value mk_val_list(paranode *vals, int num_vals) {
  CAMLparam0();
  CAMLlocal2(val1, val2);

  int i;
  if (num_vals > 0) {
    // Create the tail of the list
    val1 = caml_alloc_tuple(2);
    Store_field(val1, 0, get_value_and_remove_root(vals[num_vals-1]));
    Store_field(val1, 1, Val_int(0));

    // Extract each val and add it to the OCaml list
    for (i = num_vals - 2; i >= 0; --i) {
      val2 = caml_alloc_tuple(2);
      Store_field(val2, 0, get_value_and_remove_root(vals[i]));
      Store_field(val2, 1, val1);
      val1 = val2;
    }
  } else {
    val1 = Val_int(0);
  }
  
  CAMLreturn(val1);
}

static paranode mk_root(value v) {
  CAMLparam1(v);
  paranode_t* p = (paranode_t*)malloc(sizeof(paranode_t));
  caml_register_global_root(&(p->v));
  p->v = v;
  printf("in mk_root: %p\n", p);
  fflush(stdout);
  CAMLreturnT(paranode, p);
}

static paranode mk_node(value exp, source_info_t *src_info) {
  CAMLparam1(exp);
  CAMLlocal3(ocaml_src_info, ast_info, node);

  // build the ast_info and src_info
  ocaml_src_info = mk_src_info(src_info);

  ast_info = caml_callback(*ocaml_mk_ast_info, Val_unit);

  // build the node
  node = caml_alloc_tuple(3);
  Store_field(node, 0, exp);
  Store_field(node, 1, ocaml_src_info);
  Store_field(node, 2, ast_info);

  CAMLreturnT(paranode, mk_root(node));
}

