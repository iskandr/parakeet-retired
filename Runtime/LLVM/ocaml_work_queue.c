/*
 * ocaml_work_queue.c
 *
 * OCaml interface to parallel CPU backen.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2012.
 */
#include <llvm-c/ExecutionEngine.h>

static void execute_work_item(work_item_t *work_item) {
  LLVMGenericValueRef Result = LLVMRunFunction(work_item->ee,
                                               work_item->binary,
                                               work_item->num_args,
                                               work_item->args);

  // For now, don't return anything.
  //return Result;
}
