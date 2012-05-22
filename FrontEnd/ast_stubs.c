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
value *ocaml_mk_formal_args = NULL;
value *ocaml_mk_actual_args = NULL;

static int ast_inited = 0;
value mk_src_info(source_info_t *src_info);
paranode mk_num(value num, source_info_t *src_info);
value get_value_and_remove_root(paranode p);
paranode mk_root(value v);
paranode mk_node(value exp, source_info_t *src_info);

/** Public interface **/

void ast_init(void) {
  if (ast_inited) return;

  ast_inited = 1;

  ocaml_mk_ast_info    = caml_named_value("mk_ast_info");
  ocaml_print_ast_node = caml_named_value("print_ast_node"); 
  ocaml_get_prim       = caml_named_value("get_prim");
  ocaml_mk_formal_args = caml_named_value("mk_formal_args");
  ocaml_mk_actual_args = caml_named_value("mk_actual_args");
}


value build_str_list(char **strs, int num_strs) {
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


paranode mk_lambda(char **args, int num_args, paranode body,
                source_info_t *src_info) {
  printf("C: mk_lambda\n");
  CAMLparam0();
  CAMLlocal4(lam, args_list,  node, val_body);

  //TODO: Unbreak this by creating a formal_args object
  val_body = get_value_and_remove_root(body);
  args_list = build_str_list(args, num_args);

  // Build the lambda expression
  lam = caml_alloc(2, Exp_Lambda);
  Store_field(lam, 0, args_list);
  Store_field(lam, 1, val_body);

  // Build the node and return
  CAMLreturnT(paranode, mk_node(lam, src_info));
}

paranode mk_var(char *str, source_info_t *src_info) {

  printf("C: mk_var: %s\n", str);
  CAMLparam0();
  CAMLlocal1(var);

  // build the var expression
  var = caml_alloc(1, Exp_Var);
  Store_field(var, 0, caml_copy_string(str));

  // build the node and return
  CAMLreturnT(paranode, mk_node(var, src_info));

}

paranode mk_bool_paranode(int b, source_info_t *src_info) {
   printf("C: mk_bool: %d\n", b);
   CAMLparam0();
   CAMLlocal1(val);

   val = caml_alloc(1, PARNUM_BOOL);
   Store_field(val, 0, Val_int(b));
   CAMLreturnT(paranode, mk_node(val, src_info));
}

paranode mk_int32_paranode(int32_t i, source_info_t *src_info) {
  printf("C: mk_int32: %d\n", i);
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
  printf("C: mk_float: %f\n", f);
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
  CAMLlocal1(exp_str);

  exp_str = caml_alloc(1, Exp_Str);
  Store_field(exp_str, 0, caml_copy_string(str));

  CAMLreturnT(paranode, mk_node(exp_str, src_info));
}

paranode mk_call(
		  paranode fun,
		  paranode *args,
		  int num_args,
		  char** keywords,
		  paranode* keyword_values,
		  int num_keyword_args,
          source_info_t *src_info) {
  printf("C: mk_call %d with %d positional args and %d keyword args\n",
		  fun, num_args, num_keyword_args);
  CAMLparam0();
  CAMLlocal5(val_fun, args_list, kwd_list, kwd_values_list, app);
  CAMLlocal1(actual_args);
  printf("Making args list\n");
  args_list = mk_val_list(args, num_args);
  printf("Making keyword name list\n");
  kwd_list = build_str_list(keywords, num_keyword_args);
  printf("Making keyword values list\n");
  kwd_values_list = mk_val_list(keyword_values, num_keyword_args);
  printf("Calling into OCaml to create actual args\n");
  actual_args = \
    caml_callback3(*ocaml_mk_actual_args, args_list, kwd_list, kwd_values_list);
  printf("Creating Call node\n");
  val_fun = get_value_and_remove_root(fun);
  app = caml_alloc(2, Exp_Call);
  Store_field(app, 0, val_fun);
  Store_field(app, 1, actual_args);
  printf("Returning to Python\n");


  CAMLreturnT(paranode, mk_node(app, src_info));
}

paranode mk_return(paranode* args, int num_args, source_info_t *src_info) { 
  printf("C: ast_stubs.mk_return with %d args\n", num_args);
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
  arr = caml_alloc(1, Exp_Array);
  Store_field(arr, 0, elt_list);
  CAMLreturnT(paranode, mk_node(arr, src_info));
}

paranode mk_if(paranode cond_node, paranode true_node, paranode false_node,
               source_info_t *src_info) {
  printf("C: ast_stubs.mk_if\n");
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
  printf("C: Making you a block of %d statements\n", num_stmts);
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

  loop = caml_alloc(2, Exp_WhileLoop);
  Store_field(loop, 0, val_test);
  Store_field(loop, 1, val_body);

  CAMLreturnT(paranode, mk_node(loop, src_info));
}

paranode mk_countloop(paranode count, paranode body, source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal3(val_count, val_body, loop);

  val_count = get_value_and_remove_root(count);
  val_body  = get_value_and_remove_root(body);

  loop = caml_alloc(2, Exp_CountLoop);
  Store_field(loop, 0, val_count);
  Store_field(loop, 1, val_body);

  CAMLreturnT(paranode, mk_node(loop, src_info));
}

paranode mk_none(source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal1(v);

  v = caml_alloc(1, Exp_None);
  Store_field(v, 0, Val_int(0));
  CAMLreturnT(paranode, mk_node(v, src_info));
}

paranode get_prim(char* prim_name) {
  CAMLparam0();
  CAMLlocal1(prim);

  // build the var expression
  prim = caml_callback(*ocaml_get_prim, caml_copy_string(prim_name));

  // build the node and return
  CAMLreturnT(paranode, mk_root(prim));
}

void print_ast_node(paranode n) { 
  CAMLparam0();
  CAMLlocal1(v);
  v = ((paranode_t*)n)->v;
  caml_callback(*ocaml_print_ast_node, v);
  CAMLreturn0;
}

/** Private functions **/

paranode mk_num(value val, source_info_t *src_info) {
  CAMLparam1(val);
  CAMLlocal1(num);

  num = caml_alloc(1, Exp_Num);
  Store_field(num, 0, val);

  CAMLreturnT(paranode, mk_node(num, src_info));
}

value mk_src_info(source_info_t *src_info) {
  CAMLparam0();
  CAMLlocal3(ocaml_src_info, file, some_none);
  printf("C: Making source info");
  if (src_info != NULL) {
	printf("C: Non-null source info %p\n", src_info);
    if (src_info->filename) {
      printf("Src info filename: %s\n", src_info->filename);

      file = caml_copy_string(src_info->filename);
      //int len = strlen(src_info->filename);
      //file = caml_alloc_string(len);
      //memcpy(String_val(file),src_info->filename , len);

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
	printf("C: Null source info\n");
    ocaml_src_info = caml_alloc_tuple(3);
    Store_field(ocaml_src_info, 0, Val_int(0));
    Store_field(ocaml_src_info, 1, Val_int(0));
    Store_field(ocaml_src_info, 2, Val_int(0));
  }

  CAMLreturn(ocaml_src_info);
}

value get_value_and_remove_root(paranode node) {
  CAMLparam0();
  paranode_t* p = (paranode_t*)node;
  caml_remove_global_root(&(p->v));
  //TODO: when to free(p)?  I think it should still be here.
  CAMLreturn(p->v);
}

 value mk_val_list(paranode *vals, int num_vals) {
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

paranode mk_root(value v) {
  CAMLparam1(v);
  paranode_t* p = (paranode_t*)malloc(sizeof(paranode_t));
  caml_register_global_root(&(p->v));
  p->v = v;
  CAMLreturnT(paranode, p);
}

paranode mk_node(value exp, source_info_t *src_info) {
  printf("C: mk_node: src_info addr = %d\n", src_info);
  CAMLparam1(exp);
  CAMLlocal3(ocaml_src_info, ast_info, node);

  // build the ast_info and src_info
  printf("C: making source info\n");
  ocaml_src_info = mk_src_info(src_info);

  printf("C: mk_ast_info\n");
  ast_info = caml_callback(*ocaml_mk_ast_info, Val_unit);
  printf("-- AST INFO POINTER: %p\n", ast_info);

  printf("C: allocating node\n");
  // build the node
  node = caml_alloc_tuple(3);
  Store_field(node, 0, exp);
  Store_field(node, 1, ocaml_src_info);
  Store_field(node, 2, ast_info);

  CAMLreturnT(paranode, mk_root(node));
}
