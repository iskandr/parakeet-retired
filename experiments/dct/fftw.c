#include <fftw3.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

int bDim = 1024;

void fill_is_test(double*);

void fill_is(double* Is, int N, int SI) {
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

void usage() {
  printf("fftw [-n <n>] [-s <s>] [-i <i>] [-d <d>] [-t]\n");
  exit(-1);
}

int main(int argc, char** argv) {
  // Parameters.
  int N;
  N = 1;
  int I;
  I = 1;

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
    } else if (!strcmp(argv[i], "-i")) {
      ++i;
      if (i >= argc) {
        usage();
      }
      I = atoi(argv[i]);
      if (I < 1) {
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
    } else {
      usage();
    }
  }

  double *Is = (double*)malloc(sizeof(double) * N * bDim * bDim);
  double *Gs = (double*)malloc(sizeof(double) * N * bDim * bDim);
  //fill_is_test(Is);
  fill_is(Is, N, 1);

  struct timeval start, stop, result;

  double t;
  gettimeofday(&start, NULL);

  fftw_plan fft = fftw_plan_r2r_2d(bDim, bDim, Is, Gs,
                                   FFTW_REDFT10, FFTW_REDFT10, FFTW_ESTIMATE);
  for (i = 0; i < I; ++i) {
    fftw_execute(fft);
  }

  gettimeofday(&stop, NULL);
  timersub(&stop, &start, &result);
  t = result.tv_sec + result.tv_usec / 1000000.0;
  
  // Do scaling
  Gs[0] = Gs[0] / 32.0;
  for (i = 1; i < bDim; ++i) {
    Gs[i] = Gs[i] * sqrt(2.0) / 32.0;
  }
  for (i = bDim; i < bDim * bDim; i += bDim) {
    Gs[i] = Gs[i] * sqrt(2.0) / 32.0;
  }
  int idx;
  for (i = 1; i < bDim; ++i) {
    for (j = 1; j < bDim; ++j) {
      idx = i * bDim + j;
      Gs[idx] = Gs[idx] / 16.0;
    }
  }

  if (1) {
    printf("Time: %f\n", t);
  }
  
  /*
  for (i = 0; i < bDim; ++i) {
    for (j = 0; j < bDim; ++j) {
      printf("%9.2f ", Gs[i*bDim + j]);
    }
    printf("\n");
  }
  printf("\n");
  */
  
  free(Is);
  free(Gs);
  return 0;
}

void fill_is_test(double *Is) {
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
