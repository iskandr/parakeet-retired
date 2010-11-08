/*
 * Parallel Q Library
 *
 * (c) 2009-2010 Eric Hielscher
 *
 * Each-left each-right implementations.
 */

#include <cuda.h>
#include <stdio.h>

#include "all_pairs_dists_kernel.cu"
//#include "all_pairs_dists_1dtex_kernel.cu"
#include "base.h"
#include "divide_map_kernel.cu"
#include "equal_kernel.cu"
#include "equal_map_kernel.cu"
#include "find_min_kernel.cu"
#include "index_kernel.cu"
#include "reduce2d_kernel.cu"
#include "scan.h"
#include "where_kernel.cu"

#define THREADS_PER_BLOCK 256
#define THREADS_PER_DIM 16

#ifdef __cplusplus
extern "C" {
#endif

typedef struct kmeans_rslt {
  float *C;
  int converged;
} kmeans_rslt_t;

void usage(void);
void calc_centroids(float *X, int num_vecs, int vec_len, int *assignment,
                    int k, float *centroids);
void pieces(float *X, int num_vecs, int vec_len, int *assignment,
            int k, float *centroids);
kmeans_rslt_t kmeans(float *X, int k, int *assignment, int maxiters,
                     int num_vecs, int vec_len, int memtime);

void usage(void) {
  printf("Usage:\n"
         "--------\n"
         "-l n       : set length of vectors to be l\n"
         "-v n       : set number of vectors to be v\n"
         "-c n       : set number of centroids to be n\n"
         "-i n       : set max iterations to be n\n"
         "-m         : include memory allocs & transfers in timing\n"
         );
}

void calc_centroids(float *X, int num_vecs, int vec_len, int *assignment,
                    int k, float *centroids) {
  cudaError_t rslt;
  int *devBin;
  int *devIdxs;
  float *devMatches;
  float *devCurCentroid;
  rslt = cudaMalloc((void**)&devBin, num_vecs * sizeof(int));
  check_err(rslt, "Unable to alloc devBin");

  int num_blocks;
  int gridX, gridY;
  dim3 dimBlock(THREADS_PER_BLOCK);
  num_blocks = safe_div(num_vecs, THREADS_PER_BLOCK);
  make_linear_grid(num_blocks, &gridX, &gridY);
  dim3 dimGrid(gridX, gridY);
  cudaChannelFormatDesc intDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  cudaChannelFormatDesc floatDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindFloat);
  rslt = cudaBindTexture(0, equalMapInputTex, assignment, intDesc,
                         num_vecs * sizeof(int));
  check_err(rslt, "Unable to bind equalMapInputTex");
  rslt = cudaBindTexture(0, whereInputTex, devBin, intDesc,
                         num_vecs * sizeof(int));
  check_err(rslt, "Unable to bind whereInputTex");
  rslt = cudaBindTexture(0, indexVecsTex, X, floatDesc,
                         num_vecs * vec_len * sizeof(float));
  check_err(rslt, "Unable to bind indexVecsTex");
  int num_matches, i;
  for (i = 0; i < k; ++i) {
    // Get a binary vector for the ith centroid
    equal_map_kernel<<<dimGrid, dimBlock>>> (i, num_vecs, devBin);
    check_err(cudaGetLastError(), "Error launching equal map");

    // Prefix sum the binary vector
    scan_int(devBin, num_vecs, devBin);

    // Get back the number of matches for this centroid, and then allocate
    // memory to store the indices and then calculate the indices
    rslt = cudaMemcpy(&num_matches, devBin + num_vecs - 1, sizeof(int),
                      cudaMemcpyDeviceToHost);
    check_err(rslt, "Unable to get number of matches");
    rslt = cudaMalloc((void**)&devIdxs, num_matches * sizeof(int));
    check_err(rslt, "Unable to malloc devIdxs");
    where_kernel<<<dimGrid, dimBlock>>> (num_vecs, devIdxs);

    // Alloc memory for the matching vectors and then fill it with them
    rslt = cudaMalloc((void**)&devMatches,
                      num_matches * vec_len * sizeof(float));
    check_err(rslt, "Unable to malloc matches");
    num_blocks = safe_div(num_matches * vec_len, THREADS_PER_BLOCK);
    make_linear_grid(num_blocks, &gridX, &gridY);
    dim3 dimIndexGrid(gridX, gridY);
    rslt = cudaBindTexture(0, indexIdxsTex, devIdxs, intDesc,
                           num_matches * sizeof(int));
    check_err(rslt, "Unable to bind indexIdxsTex");
    index_kernel<<<dimIndexGrid, dimBlock>>>
      (num_vecs, vec_len, num_matches, devMatches);
    check_err(cudaGetLastError(), "Error launching index kernel");
    rslt = cudaUnbindTexture(indexIdxsTex);
    check_err(rslt, "Unable to unbind indexIdxsTex");

    // Reduce the matching vectors to their sum
    devCurCentroid = centroids + i * vec_len;
    launch_reduce2d(devMatches, num_matches, vec_len, devCurCentroid);

    // Divide the result by the number of matches to get the average
    num_blocks = safe_div(vec_len, THREADS_PER_BLOCK);
    make_linear_grid(num_blocks, &gridX, &gridY);
    dim3 dimDivideGrid(gridX, gridY);
    divide_map_kernel<<<dimDivideGrid, dimBlock>>>
      (num_matches, vec_len, devCurCentroid);

    // Free memory for this iteration
    cudaFree(devIdxs);
    cudaFree(devMatches);
  }

  // Free memory for this function
  cudaFree(devBin);
}

kmeans_rslt_t
kmeans(float *X, int k, int *assignment, int maxiters,
       int num_vecs, int vec_len, int memtime) {
  // Set up timing
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);
  struct timeval *pqstart, *pqend;

  // Copy down the input parameters
  float *devX;
  int *devAssignment;
  int *devLastAssignment;
  float *devC;
  float *devD;

  if (memtime) {
    printf("Including memory in timing results.\n");
    cudaEventRecord(start, 0);
    pqstart = pq_gettime();
  }
  
  rslt = cudaMalloc((void**)&devX, num_vecs * vec_len * sizeof(float));
  check_err(rslt, "Unable to malloc devX");
  rslt = cudaMalloc((void**)&devC, k * vec_len * sizeof(float));
  check_err(rslt, "Unable to malloc devC");
  rslt = cudaMalloc((void**)&devD, num_vecs * k * sizeof(float));
  check_err(rslt, "Unable to malloc devD");
  rslt = cudaMalloc((void**)&devAssignment, num_vecs * sizeof(int));
  check_err(rslt, "Unable to malloc devAssignment");
  rslt = cudaMalloc((void**)&devLastAssignment, num_vecs * sizeof(int));
  check_err(rslt, "Unable to malloc devLastAssignment");
  rslt = cudaMemcpy(devX, X, num_vecs * vec_len * sizeof(float),
                    cudaMemcpyHostToDevice);
  check_err(rslt, "Unable to copy to devX");
  rslt = cudaMemcpy(devAssignment, assignment, num_vecs * sizeof(int),
                    cudaMemcpyHostToDevice);
  check_err(rslt, "Unable to copy to devAssignment");

  if (!memtime) {
    printf("Not Including memory in timing results.\n");
    cudaEventRecord(start, 0);
    pqstart = pq_gettime();
  }

  // Calculate the first centroids
  calc_centroids(devX, num_vecs, vec_len, devAssignment, k, devC);
  
  cudaChannelFormatDesc floatDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindFloat);
  
  rslt = cudaBindTexture2D(0, allDistsLeftTex, devX, floatDesc,
                           vec_len, num_vecs, vec_len * sizeof(float));
  check_err(rslt, "Unable to bind allDistsLeftTex");
  rslt = cudaBindTexture2D(0, allDistsRightTex, devC, floatDesc,
                           vec_len, k, vec_len * sizeof(float));
  check_err(rslt, "Unable to bind allDistsRightTex");
  /*
  rslt = cudaBindTexture(0, allDistsLeftTex, devX, floatDesc,
                         num_vecs * vec_len * sizeof(float));
  check_err(rslt, "Unable to bind allDistsLeftTex");
  rslt = cudaBindTexture(0, allDistsRightTex, devC, floatDesc,
                         k * vec_len * sizeof(float));
  check_err(rslt, "Unable to bind allDistsRightTex");
  */
  rslt = cudaBindTexture(0, findMinInputTex, devD, floatDesc,
                         num_vecs * k * sizeof(float));
  check_err(rslt, "Unable to bind findMinInputTex");
  dim3 dimAPDBlock(THREADS_PER_DIM, THREADS_PER_DIM);
  dim3 dimAPDGrid(safe_div(k, THREADS_PER_DIM),
                  safe_div(num_vecs, THREADS_PER_DIM));
  int num_min_blocks = safe_div(num_vecs, THREADS_PER_BLOCK);
  int grid_min_x, grid_min_y;
  make_linear_grid(num_min_blocks, &grid_min_x, &grid_min_y);
  dim3 dim256Block(THREADS_PER_BLOCK);
  dim3 dimMinGrid(grid_min_x, grid_min_y);
  int converged = 0;
  int i = 0;
  int *tmp;
  while (i < maxiters && !converged) {
    // Save last assignment
    tmp = devLastAssignment;
    devLastAssignment = devAssignment;
    devAssignment = tmp;

    // Calculate distances to all the centroids
    //all_pairs_dists_1dtex_kernel<<<dimAPDGrid, dimAPDBlock>>>
    all_pairs_dists_kernel<<<dimAPDGrid, dimAPDBlock>>>
      (num_vecs, k, vec_len, devD);
    check_err(cudaGetLastError(), "Error launching all pairs dists");

    /*
    float *dists = (float*)malloc(num_vecs * k * sizeof(float));
    cudaMemcpy(dists, devD, num_vecs * k * sizeof(float),
               cudaMemcpyDeviceToHost);
    
    
    float *C = (float*)malloc(k * vec_len * sizeof(float));
    cudaMemcpy(C, devC, k * vec_len * sizeof(float),
               cudaMemcpyDeviceToHost);
    int left_idx, right_idx, x, y, l;
    float result, intermediate;
    int same = 1;
    for (y = 0; y < num_vecs; ++y) {
      for (x = 0; x < k; ++x) {
        left_idx  = vec_len * y;
        right_idx = vec_len * x;
        result = intermediate = 0.0f;
        for (l = 0; l < vec_len; ++l) {
          intermediate = X[left_idx + l] - C[right_idx + l];
          result += intermediate * intermediate;
        }
        result = sqrt(result);
        if (same &&
            fabs((result - dists[y * k + x]) / result) >= 0.001f) {
          printf("dists different for iteration %d:\n", i);
          printf("real[%d;%d]: %f\n", y, x, result);
          printf("dists[%d;%d]: %f\n", y, x, dists[y * k + x]);
          printf("real/dists: %f\n\n",
                 fabs((result - dists[y * k + x]) / result));
        }
        same &= (fabs((result - dists[y * k + x]) / result) < 0.001f);
      }
    }
    if (same) {
      printf("dists same!\n");
    }
    free(C);
    //free(dists);
    */

    // Find new assignment
    find_min_kernel<<<dimMinGrid, dim256Block>>> (num_vecs, k, devAssignment);
    check_err(cudaGetLastError(), "Error with find min");

    /*
    int *mins = (int*)malloc(num_vecs * sizeof(int));
    cudaMemcpy(mins, devAssignment, num_vecs * sizeof(int),
               cudaMemcpyDeviceToHost);
    int j;
    float min;
    int minidx;
    same = 1;
    for (j = 0; j < num_vecs; ++j) {
      min = dists[j*k];
      minidx = 0;
      for (l = 1; l < k; ++l) {
        if (dists[j*k + l] < min) {
          min = dists[j*k + l];
          minidx = l;
        }
      }
      if (same && minidx != mins[j]) {
        printf("Different mins at %d: (r,gpu) (%d,%d)\n", j, minidx, mins[j]);
        same = 0;
      }
    }
    if (same) {
      printf("Same mins!\n");
    }
    free(mins);
    free(dists);
    */

    // Calculate the new centroids
    calc_centroids(devX, num_vecs, vec_len, devAssignment, k, devC);

    // Test for convergence
    converged = equal_int(devAssignment, devLastAssignment, num_vecs);

    /*
    int *a = (int*)malloc(num_vecs * sizeof(int));
    cudaMemcpy(a, devAssignment, num_vecs * sizeof(int),
               cudaMemcpyDeviceToHost);
    int *la = (int*)malloc(num_vecs * sizeof(int));
    cudaMemcpy(la, devLastAssignment, num_vecs * sizeof(int),
               cudaMemcpyDeviceToHost);
    //int j;
    same = 1;
    for (j = 0; j < num_vecs; ++j) {
      if(same && a[j] != la[j]) {
        printf("assignments not same at idx %d: (%d,%d)\n", j, a[j], la[j]);
        same = 0;
      }
    }
    if (same) {
      printf("Assignments same!\n");
    }
    */

    ++i;
  }

  if (!memtime) {
    pqend = pq_gettime();
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
  }
  
  // Fetch the result centroids from the GPU
  kmeans_rslt_t ret;
  ret.C = (float*)malloc(k * vec_len * sizeof(float));
  rslt = cudaMemcpy(ret.C, devC, k * vec_len * sizeof(float),
                    cudaMemcpyDeviceToHost);
  check_err(rslt, "Unable to copy centroids back to host");

  if (memtime) {
    pqend = pq_gettime();
    cudaEventRecord(stop, 0);
    cudaEventSynchronize(stop);
  }

  // Calculate timings
  float gpuTime;
  cudaEventElapsedTime(&gpuTime, start, stop);
  cudaEventDestroy(start);
  cudaEventDestroy(stop);
  printf("GPU Time for K-Means: %fms\n", gpuTime);
  printf("Wall Time for K-Means: %fms\n", 1000*pq_diff_timers(pqstart, pqend));
  printf("Converged after %d iterations\n", i);
  
  cudaFree(devX);
  cudaFree(devC);
  cudaFree(devAssignment);
  cudaFree(devLastAssignment);
  
  ret.converged = converged;
  return ret;
}

#ifdef __cplusplus
}
#endif

void serial_calc_centroids(float *X, int num_vecs, int vec_len,
                           int *a, int k, float *C) {
  int i, j, l, num;
  float *centroid;
  for (i = 0; i < k; ++i) {
    num = 0;
    centroid = C + i*vec_len;
    for (j = 0; j < vec_len; ++j) {
      centroid[j] = 0.0f;
    }
    for (j = 0; j < num_vecs; ++j) {
      if (a[j] == i) {
        ++num;
        for (l = 0; l < vec_len; ++l) {
          centroid[l] += X[j*vec_len + l];
        }
      }
    }
    for (j = 0; j < vec_len; ++j) {
      centroid[j] /= num;
    }
  }
}
          
int main(int argc, char **argv) {
  int vec_len = 512;
  int num_vecs = 30000;
  int k = 10;
  int maxiters = 100;
  int memtime = 0;
  int i;

  // Process command line
  for (i = 1; i < argc; ++i) {
    if (!strcmp(argv[i], "-l")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      vec_len = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-v")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      num_vecs = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-c")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      k = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-i")) {
      if (argc < i + 2) {
        usage();
        exit(-1);
      }
      i += 1;
      maxiters = atoi(argv[i]);
    } else if (!strcmp(argv[i], "-m")) {
      memtime = 1;
    }
  }

  // Create input data
  float *X = (float*)malloc(num_vecs * vec_len * sizeof(float));
  int *assignment = (int*)malloc(num_vecs * sizeof(int));
  float d;
  for (i = 0; i < num_vecs * vec_len; ++i) {
    d = (float)rand();
    if (d < 1.0f) d = 1.0f;
    X[i] = rand() / d;
  }
  for (i = 0; i < num_vecs; ++i) {
    assignment[i] = rand() % k;
  }

  // Perform the computation
  kmeans_rslt_t rslt;
  rslt = kmeans(X, k, assignment, maxiters, num_vecs, vec_len, memtime);

  // Verify the result... ech
  float *centroids = (float*)malloc(k * vec_len * sizeof(float));
  int *a = (int*)malloc(num_vecs * sizeof(int));
  int *la = (int*)malloc(num_vecs * sizeof(int));
  for (i = 0; i < num_vecs; ++i) {
    a[i] = assignment[i];
  }
  
  i = 0;
  int j, l, m;
  int *tmp;
  int converged = 0;
  float interm, dist;
  float min;
  int minidx;
  struct timeval *ss, *se;
  ss = pq_gettime();
  serial_calc_centroids(X, num_vecs, vec_len, a, k, centroids);
  while (i < maxiters && !converged) {
    tmp = la;
    la = a;
    a = tmp;
    for (j = 0; j < num_vecs; ++j) {
      for (l = 0; l < k; ++l) {
        dist = 0.0f;
        for (m = 0; m < vec_len; ++m) {
          interm = (X[j*vec_len+m] - centroids[l*vec_len+m]);
          dist += interm * interm;
        }
        dist = sqrt(dist);
        if (l == 0) {
          min = dist;
          minidx = 0;
        } else {
          if (min > dist) {
            min = dist;
            minidx = l;
          }
        }
      }
      a[j] = minidx;
    }
    serial_calc_centroids(X, num_vecs, vec_len, a, k, centroids);
    converged = 1;
    for (j = 0; j < num_vecs; ++j) {
      if (la[j] != a[j]) {
        converged = 0;
        break;
      }
    }
    ++i;
  }
  se = pq_gettime();

  printf("Serial converged after %d iters\n", i);
  printf("Time for serial C: %fms\n", 1000*pq_diff_timers(ss, se));

  int same = 1;
  float tol = 0.01f;
  for (i = 0; i < k; ++i) {
    for (j = 0; j < vec_len; ++j) {
      if (same && fabs((rslt.C[i*vec_len + j] - centroids[i*vec_len + j]) /
                       rslt.C[i*vec_len + j]) >= tol) {
        same = 0;
        printf("CPU and GPU different for centroid %d at idx %d: (%f,%f)\n",
               i, j, rslt.C[i*vec_len + j], centroids[i*vec_len + j]);
      }
    }
  }
  if (same) {
    printf("GPU and GPU same!\n");
  }
  
  free(a);
  free(la);
  free(centroids);
  

  // Cleanup
  free(X);
  free(assignment);
  free(rslt.C);

  return 0;
}
