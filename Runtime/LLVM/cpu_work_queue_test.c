/*
 * cpu_work_queue_test.c
 *
 * Unit test for CPU work queue
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2012.
 */
#include <stdio.h>
#include <stdlib.h>

#include "cpu_work_queue.h"

int add1(int x) {
  return x + 1;
}

void execute_work(work_item_t *work_item) {
  // For now, we're just going to cast the args to be the type we want for
  // the test.  Eventually, we should add a unit test that actually uses LLVM,
  // but that will involve learning the C interface.
  int *input = (int*)work_item->args[0];
  int *output = (int*)work_item->args[1];
  int i;
  for (i = 0; i < work_item->num_args; ++i) {
    output[i] = add1(input[i]);
  }
}

int test_work() {
  cpu_work_queue_t *work_queue = create_work_queue(8, &execute_work);

  int input[16];
  int output[16];

  int i;
  for (i = 0; i < 16; ++i) {
    input[i] = i;
    output[i] = 0;
  }

  work_item_t *work_items = (work_item_t*)malloc(8*sizeof(work_item_t));

  for (i = 0; i < 8; ++i) {
    work_items[i].args =
        (LLVMGenericValueRef*)malloc(2*sizeof(LLVMGenericValueRef));
    work_items[i].args[0] = (LLVMGenericValueRef)(input + 2*i);
    work_items[i].args[1] = (LLVMGenericValueRef)(output + 2*i);
    work_items[i].num_args = 2;
  }

  do_work(work_queue, work_items, 8);

  int success = 1;
  for (i = 0; i < 16; ++i) {
    success = success && add1(input[i]) == output[i];
  }

  destroy_work_queue(work_queue);
  return success;
}

int main(int argc, char **argv) {
  if (test_work()) {
    printf("Queue test successful!\n");
  } else {
    printf("Queue test failed!\n");
  }

  return 0;
}
