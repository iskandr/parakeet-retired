/*
 * cpu_work_queue.c
 *
 * Implements a backend for executing work in parallel on multicore CPUs.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2012.
 */
#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#include "cpu_work_queue.h"

typedef struct {
  cpu_work_queue_t *work_queue;
  int id;
} worker_args_t;

static void *worker(void *args);

cpu_work_queue_t *create_work_queue(int num_workers, execution_function_t ef) {
  assert(num_workers > 0);

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
    printf("Couldn't malloc worker mutexes for LLVM work queue. Exiting.\n");
    exit(-1);
  }
  work_queue->work_available =
      (pthread_cond_t*)malloc(num_workers*sizeof(pthread_cond_t));
  if (!work_queue->work_available) {
    printf("Couldn't malloc worker conds for LLVM work queue. Exiting.\n");
    exit(-1);
  }
  work_queue->num_workers = num_workers;
  pthread_barrier_init(&work_queue->barrier, NULL, num_workers + 1);
  pthread_mutex_init(&work_queue->mutex, NULL);
  pthread_cond_init(&work_queue->queue_signal, NULL);
  work_queue->status = WQ_STATUS_RUN;
  work_queue->work_items  = NULL;
  work_queue->execution_function = ef;

  int i, rc;
  for (i = 0; i < num_workers; ++i) {
    pthread_mutex_init(&work_queue->worker_mutexes[i], NULL);
    pthread_cond_init(&work_queue->work_available[i], NULL);
    worker_args_t *args = (worker_args_t*)malloc(sizeof(worker_args_t));
    args->work_queue = work_queue;
    args->id = i;
    pthread_mutex_lock(&work_queue->mutex);
    rc = pthread_create(&work_queue->workers[i], NULL,
                        worker, (void*)args);
    if (rc) {
      printf("Couldn't create worker %d (Error code %d). Exiting.\n", i, rc);
      exit(-1);
    }
    pthread_cond_wait(&work_queue->queue_signal, &work_queue->mutex);
    pthread_mutex_unlock(&work_queue->mutex);
  }

  return work_queue;
}

wq_ret_t destroy_work_queue(cpu_work_queue_t *work_queue) {
  if (!work_queue) {
    printf("Can't destroy NULL work queue. Exiting.\n");
    exit(-1);
  }

  // Wake up all the threads, telling them that we're shutting down.
  work_queue->status = WQ_STATUS_STOP;
  int i;
  for (i = 0; i < work_queue->num_workers; ++i) {
    pthread_mutex_lock(&work_queue->worker_mutexes[i]);
    pthread_cond_signal(&work_queue->work_available[i]);
    pthread_mutex_unlock(&work_queue->worker_mutexes[i]);
  }

  // Free memory and return.
  for (i = 0; i < work_queue->num_workers; ++i) {
    pthread_mutex_destroy(&work_queue->worker_mutexes[i]);
    pthread_cond_destroy(&work_queue->work_available[i]);
  }
  pthread_barrier_destroy(&work_queue->barrier);
  pthread_mutex_destroy(&work_queue->mutex);
  pthread_cond_destroy(&work_queue->queue_signal);
  free(work_queue->workers);
  free(work_queue->worker_mutexes);
  free(work_queue->work_available);
  free(work_queue);
  return WQ_SUCCESS;
}

wq_ret_t do_work(cpu_work_queue_t *work_queue,
                 work_item_t *work_items, int num_items) {
  // For now, assert that the number of items == number of workers.
  // TODO: Later, we'll likely want to change this.
  assert(num_items == work_queue->num_workers);

  // Add the work to the queue.
  work_queue->work_items = work_items;

  // Notify the workers of the work and wait for each to ack.
  int i;
  for (i = 0; i < work_queue->num_workers; ++i) {
    pthread_mutex_lock(&work_queue->worker_mutexes[i]);
    pthread_cond_signal(&work_queue->work_available[i]);
    pthread_mutex_unlock(&work_queue->worker_mutexes[i]);
  }

  // Wait for the work to be completed.
  int rc = pthread_barrier_wait(&work_queue->barrier);
  if (rc && rc != PTHREAD_BARRIER_SERIAL_THREAD) {
    printf("Couldn't wait on barrier in LLVM work queue. Exiting.\n");
    exit(-1);
  }
 
  // Free the work_items.
  free(work_queue->work_items);
}

static void *worker(void *args) {
  cpu_work_queue_t *work_queue = ((worker_args_t*)args)->work_queue;
  int id = ((worker_args_t*)args)->id;
  pthread_mutex_t *my_mutex = &work_queue->worker_mutexes[id];
  free(args);

  // Setup needed to ensure that no work is issued until the workers are
  // waiting for it.
  pthread_mutex_lock(my_mutex);
  pthread_mutex_lock(&work_queue->mutex);
  pthread_cond_signal(&work_queue->queue_signal);
  pthread_mutex_unlock(&work_queue->mutex);

  for (;;) {
    // Lock the queue mutex and wait for work.
    pthread_cond_wait(&work_queue->work_available[id], my_mutex);

    // Make sure we're not shutting down.
    if (work_queue->status == WQ_STATUS_STOP) {
      break;
    }

    // Grab my work from the pool and then release the lock so that other
    // threads can do work in parallel.
    work_item_t *work_item = &work_queue->work_items[id];
    pthread_mutex_unlock(my_mutex);

    // Do the work.
    (*work_queue->execution_function)(work_item);

    // Grab my mutex again so that I can wait.
    pthread_mutex_lock(my_mutex);

    // Signal that my work is done.
    // TODO: If/when we implement work stealing, this will have to change.
    int rc = pthread_barrier_wait(&work_queue->barrier);
    if (rc && rc != PTHREAD_BARRIER_SERIAL_THREAD) {
      printf("Couldn't wait on barrier in LLVM work queue. Exiting.\n");
      exit(-1);
    }
  }

  // Tell the master thread that we got the shutdown signal and then exit.
  pthread_mutex_unlock(my_mutex);
  return NULL;
}
