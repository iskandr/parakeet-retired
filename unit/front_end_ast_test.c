#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>
#include <stdio.h>

#include "ast_stubs.h"
#include "dyn_type_stubs.h"
#include "front_end_stubs.h"
#include "host_val_stubs.h"
#include "parakeet.h"
#include "prim_variants.h"

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

/*
 * We'll build and try to execute an AST for the function:
 *
 * def add2(x):
 *   return x + 2
 *
 * The Parakeet AST ends up being:
 *
 * App(Prim(Scalar_Op_Add), [Var("x"), Num(PQNum.Int64(2))])
 */

int main(int argc, char **argv) {
  parakeet_init();

  // Build AST for the function body
  paranode plus_args[2];
  plus_args[0] = mk_var("x", NULL);
  plus_args[1] = mk_int32_paranode(2, NULL);

  paranode plus     = mk_scalar_op(Scalar_Op_Add, NULL);
  paranode app_plus = mk_app(plus, plus_args, 2, NULL);

  paranode plus_block = mk_block(&app_plus, 1, NULL);

  char *args[1] = {"x"};

  // Register the function
  int add2id = register_untyped_function("add2", NULL, 0, args, 1, plus_block);

  // Build an input value for x
  int input_data[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  int input_shape[1] = {10};
  dyn_type scalar_int = mk_scalar(Int32T);
  dyn_type vec_int = mk_vec(scalar_int);
  host_val input = mk_host_array((char*)input_data, vec_int, input_shape, 1,
                                 10 * sizeof(int));
  host_val inputs[1];
  inputs[0] = input;

  // Run the function with x = [0 1 2 3 4 5 6 7 8 9]
  return_val_t ret = run_function(add2id, NULL, 0, inputs, 1);

  int i;
  if (ret.return_code == RET_SUCCESS) {
    // Hard-coding the expected return type to be Vec(Int)
    int *rslt = (int*)ret.data.results[0];
    for (i = 0; i < 10; ++i) {
      printf("%d ", rslt[i]);
    }
    printf("\n");
  } else {
    printf("Error msg: %s\n", ret.data.error_msg);
  }

  // I ignore returned memory cleanup for now, but I should probably do it
  // just to see how it should be done
  return 0;
}

