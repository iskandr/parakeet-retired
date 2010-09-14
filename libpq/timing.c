#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "pqlib.h"

char ptx_code[] = "\n\
  .version 1.3\n\
  .target sm_13\n\
  .entry add2Kernel\n\
  {\n\
  \n\
  .reg          .u16 %rh<4>;\n\
  .reg          .u32 %r<12>;\n\
  .reg          .u64 %rd<9>;\n\
  .reg          .pred %p1;\n\
  \n\
  .param        .u64 output_array;\n\
  .param        .u64 x_array;\n\
  .param        .u64 y_array;\n\
  .param        .s32 n;\n\
  \n\
  mov.u16       %rh1, %ntid.x;\n\
  mov.u16       %rh2, %ctaid.x;\n\
  mul.wide.u16  %r1, %rh2, %rh1;\n\
  mov.u16       %rh3, %nctaid.x;\n\
  mul.wide.u16  %r2, %rh3, %rh1;\n\
  cvt.u32.u16   %r3, %ctaid.y;\n\
  mul.lo.u32    %r4, %r3, %r2;\n\
  add.u32       %r5, %r1, %r4;\n\
  cvt.u32.u16   %r6, %tid.x;\n\
  add.u32       %r7, %r6, %r5;\n\
  \n\
  ld.param.s32  %r8, [n];\n\
  setp.le.s32   %p1, %r8, %r7;\n\
  @%p1 bra      $Out_of_bounds;\n\
  \n\
  cvt.u64.s32   %rd1, %r7;\n\
  mul.lo.u64    %rd2, %rd1, 4;\n\
  ld.param.u64  %rd3, [x_array];\n\
  add.u64       %rd4, %rd3, %rd2;\n\
  ld.global.s32 %r9, [%rd4+0];\n\
  ld.param.u64  %rd5, [y_array];\n\
  add.u64       %rd6, %rd5, %rd2;\n\
  ld.global.s32 %r10, [%rd6+0];\n\
  \n\
  add.s32       %r11, %r9, %r10;\n\
  \n\
  ld.param.u64  %rd7, [output_array];\n\
  add.u64       %rd8, %rd7, %rd2;\n\
  st.global.s32 [%rd8+0], %r11;\n\
  \n\
  $Out_of_bounds:\n\
  exit;\n\
  }\n\
";

char emptyKernel[] = "\n\
  .version 1.3\n\
  .target sm_13\n\
  .entry emptyKernel\n\
  {\n\
  \n\
  .reg          .u32 %r0;\n\
  \n\
  exit;\n\
  }\n\
";

void test_kernel(void) {
  struct timespec *start, *end;

  int ar_size = 1000000;

  int mem_size = sizeof(int)*ar_size;
  int *x_array   = (int*)malloc(mem_size);
  int *y_array   = (int*)malloc(mem_size);
  int *out_array = (int*)malloc(mem_size);
  int i;

  for (i = 0; i < ar_size; ++i) {
    x_array[i] = (rand() % 30);
    y_array[i] = (rand() % 30);
  }

  printf("Arrays initialized.\n");

  CUdeviceptr cuda_x_array;
  cuMemAlloc(&cuda_x_array, mem_size);
  CUdeviceptr cuda_y_array;
  cuMemAlloc(&cuda_y_array, mem_size);

  printf("Cuda memory alloc'ed.\n");

  cuMemcpyHtoD(cuda_x_array, x_array, mem_size);
  cuMemcpyHtoD(cuda_y_array, y_array, mem_size);

  printf("Arrays copied to device.\n");

  start = pq_gettime();
  CUmodule *cuModule = pq_compile_module(ptx_code);
  end = pq_gettime();

  printf("Ptx compiled in %10f seconds.\n", pq_diff_timers(start, end));

  CUdeviceptr kernel_args[3];
  kernel_args[0] = cuda_x_array;
  kernel_args[1] = cuda_x_array;
  kernel_args[2] = cuda_y_array;

  start = pq_gettime();
  pq_launch_ptx(cuModule, "add2Kernel", kernel_args, 3, ar_size,
                256, 1, 1, 4096, 1);
  cuCtxSynchronize();
  end = pq_gettime();

  printf("Kernel launched in %10f seconds.\n", pq_diff_timers(start, end));

  cuMemcpyDtoH(out_array, cuda_x_array, mem_size);

  printf("Output array copied to host.\n");

  int same = 1;
  for (i = 0; i < ar_size; ++i) {
    same &= ((x_array[i] + y_array[i]) == out_array[i]);
  }

  if (same) {
    printf("Same!\n");
  } else {
    printf("Different!\n");
  }

  pq_destroy_module(cuModule);
  cuMemFree(cuda_x_array);
  cuMemFree(cuda_y_array);
  free(x_array);
  free(y_array);
  free(out_array);
}

void run_timer(int num_bytes) {
  double mem_time = pq_time_mem_transfer(num_bytes);
  printf("Mem txfer time %12d bytes: %10f\n", num_bytes, mem_time);
}

int main(int argc, char** argv) {
  struct timespec *start, *end;

  pq_init();

  CUcontext ctx = 0;
  cuCtxCreate(&ctx, 0, 0);

  test_kernel();

  int num_bytes = 2;
  int i;
  for (i = 2; i < 29; ++i) {
    num_bytes *= 2;
    run_timer(num_bytes);
  }
  run_timer(num_bytes * 1.5);

  start = pq_gettime();
  CUmodule *cuModule = pq_compile_module(emptyKernel);
  end = pq_gettime();
  printf("Time to compile empty kernel module: %10f\n",
         pq_diff_timers(start, end));

  start = pq_gettime();
  pq_launch_ptx_noarr(cuModule, "emptyKernel", NULL, 0, 256, 1, 1, 4096, 1);
  cuCtxSynchronize();
  end = pq_gettime();
  pq_destroy_module(cuModule);

  printf("Empty kernel launch time: %10f\n", pq_diff_timers(start, end));



  return 0;
}

