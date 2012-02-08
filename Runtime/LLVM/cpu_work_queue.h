/*
 * cpu_work_queue.h
 *
 * Implements a backend for executing work in parallel on multicore CPUs.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2012.
 */
#ifndef _CPU_WORK_QUEUE_H_
#define _CPU_WORK_QUEUE_H_

#include <llvm-c/ExecutionEngine.h>
#include <pthread.h>

typedef struct {
  LLVMExecutionEngineRef ee;
  LLVMValueRef binary;
  LLVMGenericValueRef *args;
  int num_args;
} work_item_t;

typedef struct {
  pthread_t        *workers;
  pthread_mutex_t  *worker_mutexes;
  int               num_workers;
  pthread_barrier_t barrier;
  pthread_cond_t    work_available;
  pthread_cond_t    work_finished;
  pthread_mutex_t   queue_mutex;
  work_item_t      *work_items;
} cpu_work_queue_t;

typedef enum {
  WQ_SUCCESS = 0,
  WQ_ERROR,
} wq_ret_t;

cpu_work_queue_t* create_work_queue(int num_workers);
wq_ret_t destroy_work_queue(cpu_work_queue_t* work_queue);

wq_ret_t do_work(cpu_work_queue_t* work_queue,
                 work_item_t* work_items, int num_items);

#endif

