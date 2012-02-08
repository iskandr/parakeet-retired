/*
 * cpu_work_queue.c
 *
 * Implements a backend for executing work in parallel on multicore CPUs.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2012.
 */
#include <llvm-c/ExecutionEngine.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#include "cpu_work_queue.h"

typedef struct {
  cpu_work_queue_t *work_queue;
  int id;
} worker_args_t;

static void *worker(void *args);
static void execute_work_item(work_item_t *work_item);

cpu_work_queue_t *create_work_queue(int num_workers) {
  cpu_work_queue_t *work_queue =
      (cpu_work_queue_t*)malloc(sizeof(cpu_work_queue_t));

  work_queue->workers = (pthread_t*)malloc(num_workers*sizeof(pthread_t));
  if (!work_queue->workers) {
    printf("Couldn't malloc workers for LLVM work queue. Exiting.\n");
    exit(-1);
  }
  work_queue->worker_mutexes =
      (pthread_mutex_t*)malloc(num_workers*sizeof(pthread_mutex_t));
  if (!work_queue->worker_mutexes) {
    printf("Coudln't malloc worker mutexes for LLVM work queue. Exiting.\n");
    exit(-1);
  }
  int i, rc;
  for (i = 0; i < num_workers; ++i) {
    rc = pthread_create(&work_queue->workers[i], NULL,
                        worker, (void*)work_queue);
    if (rc) {
      printf("Couldn't create worker %d (Error code %d). Exiting.\n", i, rc);
      exit(-1);
    }
    pthread_mutex_init(&work_queue->worker_mutexes[i], NULL);
  }

  work_queue->num_workers = num_workers;
  pthread_barrier_init(&work_queue->barrier, NULL, num_workers);
  pthread_cond_init(&work_queue->work_available, NULL);
  pthread_cond_init(&work_queue->work_finished, NULL);
  pthread_mutex_init(&work_queue->queue_mutex, NULL);
  work_queue->work_items  = NULL;

  return work_queue;
}

wq_ret_t destroy_work_queue(cpu_work_queue_t *work_queue) {
  return WQ_SUCCESS;
}

wq_ret_t do_work(cpu_work_queue_t *work_queue,
                 work_item_t *work_items, int num_items) {
  // For now, assert that the number of items == number of workers.
  assert(num_items == work_queue->num_workers);

  // Notify the workers of the work and wait for it to complete.
  pthread_mutex_lock(&work_queue->queue_mutex);

  work_queue->work_items = work_items;

  pthread_cond_broadcast(&work_queue->work_available);
  pthread_cond_wait(&work_queue->work_finished, &work_queue->queue_mutex);

  // All done.
  pthread_mutex_unlock(&work_queue->queue_mutex);
}

static void *worker(void *args) {
  cpu_work_queue_t *work_queue = ((worker_args_t*)args)->work_queue;
  int id = ((worker_args_t*)args)->id;
  pthread_mutex_t *my_mutex = &work_queue->worker_mutexes[id];

  for (;;) {
    // Lock my mutex and wait for work.
    pthread_mutex_lock(my_mutex);
    pthread_cond_wait(&work_queue->work_available, my_mutex);

    // Do the work.
    execute_work_item(&work_queue->work_items[id]);

    // Wait for all threads to finish, and if I'm the 0th worker, signal to
    // the queue that the work is done.
    int rc = pthread_barrier_wait(&work_queue->barrier);
    if (rc && rc != PTHREAD_BARRIER_SERIAL_THREAD) {
      printf("Couldn't wait on barrier in LLVM work queue. Exiting.\n");
      exit(-1);
    }
    if (id == 0) {
      pthread_mutex_lock(&work_queue->queue_mutex);
      pthread_cond_signal(&work_queue->work_finished);
      pthread_mutex_unlock(&work_queue->queue_mutex);
    }
  }

  return NULL;
}

static void execute_work_item(work_item_t *work_item) {
  LLVMGenericValueRef Result = LLVMRunFunction(work_item->ee,
                                               work_item->binary,
                                               work_item->num_args,
                                               work_item->args);

  // For now, don't return anything.
  //return Result;
}

