/*
 * ocaml_work_queue.c
 *
 * OCaml interface to parallel CPU backend.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2012.
 */
#include <assert.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <stdio.h>

#include "llvm_multithreading.h"

#include "cpu_work_queue.h"

static int inited = 0;
static int ocaml_list_length(value ocaml_list);

void execute_work_item(work_item_t *work_item) {
  LLVMGenericValueRef Result = LLVMRunFunction(work_item->ee,
                                               work_item->binary,
                                               work_item->num_args,
                                               work_item->args);
}

value ocaml_create_work_queue(value num_threads) {
  CAMLparam1(num_threads);

  if (!inited) {
    LLVMStartMultithreaded();
    inited = 1;
  }

  cpu_work_queue_t *work_queue =
      create_work_queue(Int_val(num_threads), &execute_work_item);

  CAMLreturn(caml_copy_int64((int64_t)work_queue));
}

value ocaml_destroy_work_queue(value ocaml_work_queue) {
  CAMLparam1(ocaml_work_queue);

  if (inited) {
    LLVMStopMultithreaded();
    inited = 0;
  }

  cpu_work_queue_t *work_queue = (cpu_work_queue_t*)Int64_val(ocaml_work_queue);
  destroy_work_queue(work_queue);

  CAMLreturn(Val_unit);
}

value ocaml_do_work(value ocaml_work_queue,
                    LLVMExecutionEngineRef ee,
                    LLVMValueRef binary,
                    value ocaml_work_item_list) {
  CAMLparam2(ocaml_work_queue, ocaml_work_item_list);
  CAMLlocal3(ocaml_cur_work_item, ocaml_arg_list, ocaml_cur_arg);
  cpu_work_queue_t *work_queue = Int64_val(ocaml_work_queue);

  // Convert the OCaml work_list into the C work_list.
  int len = ocaml_list_length(ocaml_work_item_list);
  assert(len > 0);
  work_item_t *work_list = (work_item_t*)malloc(len * sizeof(work_item_t));
  int i, j;
  ocaml_cur_work_item = ocaml_work_item_list;
  for (i = 0; i < len; ++i) {
    work_list[i].ee = ee;
    work_list[i].binary = binary;
    ocaml_arg_list = Field(ocaml_cur_work_item, 0);
    work_list[i].num_args = ocaml_list_length(ocaml_arg_list);
    work_list[i].args = (LLVMGenericValueRef*) 
        malloc(work_list[i].num_args * sizeof(LLVMGenericValueRef));
    ocaml_cur_arg = ocaml_arg_list;
    for (j = 0; j < work_list[i].num_args; ++j) {
      work_list[i].args[j] =
          *(LLVMGenericValueRef*)(Data_custom_val(Field(ocaml_cur_arg, 0)));
      ocaml_cur_arg = Field(ocaml_cur_arg, 1);
    }
    ocaml_cur_work_item = Field(ocaml_cur_work_item, 1);
  }

  // Do the work.  The queue takes ownership of the work_item array.
  do_work(work_queue, work_list, len);

  CAMLreturn(Val_unit);
}

static int ocaml_list_length(value ocaml_list) {
  CAMLparam1(ocaml_list);
  CAMLlocal1(cur);

  int i = 0;
  cur = ocaml_list;
  while (cur != Val_int(0)) {
    cur = Field(cur, 1);
    ++i;
  }
  CAMLreturnT(int, i);
}
