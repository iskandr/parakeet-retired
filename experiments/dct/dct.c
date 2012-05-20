#include <math.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

int bDim = 8;
const double pi = 3.141592653589793;
double *cosDict;

void fill_is_test(signed char *);

inline double alpha(signed char n) {
  return n == 0 ? 0.353553390593 : 0.5;
}

void naive_seq_dct(signed char *Is, int N, int SI, double *Gs, int stride) {
  int i, s, u, v, x, y;
  signed char *curI;
  double *curG;
  double cosY;
  for (i = 0; i < N; ++i) {
    for (s = 0; s < SI; ++s) {
      for (v = 0; v < bDim; ++v) {
        for (u = 0; u < bDim; ++u) {
          curI = &Is[i*stride + s*bDim*bDim];
          curG = &Gs[i*stride + s*bDim*bDim + v*bDim + u];
          *curG = 0.0;
          for (y = 0; y < bDim; ++y) {
            cosY = cosDict[y*bDim + v];
            for (x = 0; x < bDim; ++x) {
              (*curG) += curI[y*bDim + x]*cosY*cosDict[x*bDim + u];
            }
          }
          (*curG) *= alpha(u)*alpha(v);
        }
      }
    }
  }
}

typedef struct {
  pthread_barrier_t *barrier;
  signed char *Is;
  double *Gs;
  int N;
  int SI;
  int stride;
} mt_params_t;

typedef void* (*mt_work_func)(void*);

void *naive_mt_worker(void *args) {
  mt_params_t *my_args = (mt_params_t*)args;

  naive_seq_dct(my_args->Is, my_args->N, my_args->SI, my_args->Gs,
                my_args->stride);
  
  pthread_barrier_wait(my_args->barrier);
  return NULL;
}

void mt_split_imgs(signed char *Is, int N, int SI, double *Gs, mt_work_func f) {
  int num_threads = 8;
  
  pthread_barrier_t barrier;
  pthread_barrier_init(&barrier, NULL, num_threads + 1);
  
  mt_params_t *params =
    (mt_params_t*)malloc(sizeof(mt_params_t) * num_threads);
  int i;
  int nOff = (N / num_threads) * SI * bDim * bDim;
  for (i = 0; i < num_threads; ++i) {
    params[i].Is = Is + i * nOff;
    params[i].Gs = Gs + i * nOff;
    if (i < num_threads - 1) {
      params[i].N = (N / num_threads);
    } else {
      params[i].N = N - (N / num_threads) * (num_threads - 1);
    }
    params[i].SI = SI;
    params[i].stride = SI * bDim * bDim;
    params[i].barrier = &barrier;
  }

  pthread_t *threads = (pthread_t*)malloc(sizeof(pthread_t) * num_threads);
  for (i = 0; i < num_threads; ++i) {
    pthread_create(&threads[i], NULL, f, (void*)&params[i]);
    pthread_detach(threads[i]);
  }
  pthread_barrier_wait(&barrier);

  pthread_barrier_destroy(&barrier);
  free(params);
  free(threads);
}

void mt_split_sis(signed char *Is, int N, int SI, double *Gs, mt_work_func f) {
  int num_threads = 8;
  
  pthread_barrier_t barrier;
  pthread_barrier_init(&barrier, NULL, num_threads + 1);
  
  mt_params_t *params =
    (mt_params_t*)malloc(sizeof(mt_params_t) * num_threads);
  pthread_t *threads = (pthread_t*)malloc(sizeof(pthread_t) * num_threads);

  int i, n;
  for (n = 0; n < N; ++n) {
    int nOff = n * SI * bDim * bDim;
    int sOff = (SI / num_threads) * bDim * bDim;
    for (i = 0; i < num_threads; ++i) {
      params[i].Is = Is + nOff + i * sOff;
      params[i].Gs = Gs + nOff + i * sOff;
      params[i].N  = 1;
      if (i < num_threads - 1) {
        params[i].SI = (SI / num_threads);
      } else {
        params[i].SI = SI - (SI / num_threads) * (num_threads - 1);
      }
      params[i].stride = SI * bDim * bDim;
      params[i].barrier = &barrier;
    }

    for (i = 0; i < num_threads; ++i) {
      pthread_create(&threads[i], NULL, f, (void*)&params[i]);
      pthread_detach(threads[i]);
    }
    pthread_barrier_wait(&barrier);
  }

  pthread_barrier_destroy(&barrier);
  free(params);
  free(threads);
}

void test_output(signed char *Is, int N, int SI, double *Gs) {
  double *ref = (double*)malloc(sizeof(double) * N * SI * bDim * bDim);
  naive_seq_dct(Is, N, SI, ref, SI*bDim*bDim);
  
  double err = 0.0;
  int i, s, j, k;
  for (i = 0; i < N; ++i) {
    for (s = 0; s < SI; ++s) {
      for (j = 0; j < bDim; ++j) {
        for (k = 0; k < bDim; ++k) {
          err += abs(Gs[i*SI*bDim*bDim + s*bDim*bDim + j*bDim + k] -
                     ref[i*SI*bDim*bDim + s*bDim*bDim + j*bDim + k]);
        }
      }
    }
  }
  printf ("Total error: %f\n", err);
  
  free(ref);
}

void usage() {
  printf("dct [-n <n>] [-s <s>] [-d <d>] [-t|-nv|-nmti|-nmts] [-c]\n");
  exit(-1);
}

typedef enum {
  TEST = 0,
  NAIVE,
  NAIVE_MT_SPLIT_IMGS,
  NAIVE_MT_SPLIT_SUBIMGS
} method_t;

void fill_cos_dict() {
  cosDict = (double*)malloc(sizeof(double) * bDim * bDim);
  
  int i, j;
  for (i = 0; i < bDim; ++i) {
    for (j = 0; j < bDim; ++j) {
      cosDict[i*bDim + j] = cos((pi/16)*(2*i+1.0)*j);
    }
  }
}

void fill_is(signed char* Is, int N, int SI) {
  int i, s, j, k;
  for (i = 0; i < N; ++i) {
    for (s = 0; s < SI; ++s) {
      for (j = 0; j < bDim; ++j) {
        for (k = 0; k < bDim; ++k) {
          Is[i*SI*bDim*bDim + s*bDim*bDim + j*bDim + k] = rand() % 256 - 128;
        }
      }
    }
  }
}

int main(int argc, char** argv) {
  // Parameters.
  int SI, N;
  N = 32;
  SI = 40000;

  method_t method = NAIVE;
  int check_error = 0;
  
  int i, j;
  for (i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "-n")) {
      ++i;
      if (i >= argc) {
        usage();
      }
      N = atoi(argv[i]);
      if (N < 1) {
        usage();
      }
    } else if (!strcmp(argv[i], "-s")) {
      ++i;
      if (i >= argc) {
        usage();
      }
      SI = atoi(argv[i]);
      if (SI < 1) {
        usage();
      }
    } else if (!strcmp(argv[i], "-d")) {
      ++i;
      if (i >= argc) {
        usage();
      }
      bDim = atoi(argv[i]);
      if (bDim < 1) {
        usage();
      }
    } else if (!strcmp(argv[i], "-t")) {
      method = TEST;
    } else if (!strcmp(argv[i], "-nv")) {
      method = NAIVE;
    } else if (!strcmp(argv[i], "-nmti")) {
      method = NAIVE_MT_SPLIT_IMGS;
    } else if (!strcmp(argv[i], "-nmts")) {
      method = NAIVE_MT_SPLIT_SUBIMGS;
    } else if (!strcmp(argv[i], "-c")) {
      check_error = 1;
    } else {
      usage();
    }
  }

  if (method == TEST) {
    N = SI = 1;
  }
  signed char *Is = (signed char*)malloc(N * SI * bDim * bDim);
  double *Gs = (double*)malloc(sizeof(double) * N * SI * bDim * bDim);
  fill_cos_dict();
  
  if (method == TEST) {
    fill_is_test(Is);
  } else {
    fill_is(Is, N, SI);
  }
  
  struct timeval start, stop, result;

  double t;
  gettimeofday(&start, NULL);
  
  switch(method) {
    case TEST:
      for (i = 0; i < 100000; ++i) {
        naive_seq_dct(Is, N, SI, Gs, SI * bDim * bDim);
      }
      break;
    case NAIVE:
      naive_seq_dct(Is, N, SI, Gs, SI * bDim * bDim);
      break;
    case NAIVE_MT_SPLIT_IMGS:
      mt_split_imgs(Is, N, SI, Gs, &naive_mt_worker);
      break;
    case NAIVE_MT_SPLIT_SUBIMGS:
      mt_split_sis(Is, N, SI, Gs, &naive_mt_worker);
      break;
  }

  gettimeofday(&stop, NULL);
  timersub(&stop, &start, &result);
  t = result.tv_sec + result.tv_usec / 1000000.0;

  if (1) {
    printf("Time: %f\n", t);
    if (check_error) {
      test_output(Is, N, SI, Gs);
    }
  }
  
  if (method == TEST) {
    for (i = 0; i < bDim; ++i) {
      for (j = 0; j < bDim; ++j) {
        printf("%7.2f ", Gs[i*bDim + j]);
      }
      printf("\n");
    }
    printf("\n");
    for (i = 0; i < bDim; ++i) {
      for (j = 0; j < bDim; ++j) {
        printf("%4d ", Is[i*bDim + j]);
      }
      printf("\n");
    }
  }
  
  free(Is);
  free(Gs);
  free(cosDict);
  return 0;
}

void fill_is_test(signed char *Is) {
  Is[0]  = -76;
  Is[1]  = -73;
  Is[2]  = -67;
  Is[3]  = -62;
  Is[4]  = -58;
  Is[5]  = -67;
  Is[6]  = -64;
  Is[7]  = -55;
  Is[8]  = -65;
  Is[9]  = -69;
  Is[10] = -73;
  Is[11] = -38;
  Is[12] = -19;
  Is[13] = -43;
  Is[14] = -59;
  Is[15] = -56;
  Is[16] = -66;
  Is[17] = -69;
  Is[18] = -60;
  Is[19] = -15;
  Is[20] =  16;
  Is[21] = -24;
  Is[22] = -62;
  Is[23] = -55;
  Is[24] = -65;
  Is[25] = -70;
  Is[26] = -57;
  Is[27] =  -6;
  Is[28] =  26;
  Is[29] = -22;
  Is[30] = -58;
  Is[31] = -59;
  Is[32] = -61;
  Is[33] = -67;
  Is[34] = -60;
  Is[35] = -24;
  Is[36] =  -2;
  Is[37] = -40;
  Is[38] = -60;
  Is[39] = -58;
  Is[40] = -49;
  Is[41] = -63;
  Is[42] = -68;
  Is[43] = -58;
  Is[44] = -51;
  Is[45] = -60;
  Is[46] = -70;
  Is[47] = -53;
  Is[48] = -43;
  Is[49] = -57;
  Is[50] = -64;
  Is[51] = -69;
  Is[52] = -73;
  Is[53] = -67;
  Is[54] = -63;
  Is[55] = -45;
  Is[56] = -41;
  Is[57] = -49;
  Is[58] = -59;
  Is[59] = -60;
  Is[60] = -63;
  Is[61] = -52;
  Is[62] = -50;
  Is[63] = -34;
}
