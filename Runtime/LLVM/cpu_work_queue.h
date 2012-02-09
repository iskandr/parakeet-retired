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

typedef void (*execution_function_t)(work_item_t*);

typedef enum {
  WQ_STATUS_RUN = 0,
  WQ_STATUS_STOP,
} wq_status_t;

typedef struct {
  pthread_t            *workers;
  pthread_cond_t       *work_available;
  int                   num_workers;
  pthread_barrier_t     barrier;
  pthread_cond_t        msg_received;
  pthread_cond_t        work_finished;
  pthread_mutex_t       mutex;
  wq_status_t           status;
  work_item_t          *work_items;
  execution_function_t  execution_function;
} cpu_work_queue_t;

typedef enum {
  WQ_SUCCESS = 0,
  WQ_ERROR,
} wq_ret_t;

cpu_work_queue_t* create_work_queue(int num_workers, execution_function_t ef);
wq_ret_t destroy_work_queue(cpu_work_queue_t* work_queue);

wq_ret_t do_work(cpu_work_queue_t* work_queue,
                 work_item_t* work_items, int num_items);

#endif
