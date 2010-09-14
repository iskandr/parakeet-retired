#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "pqlib.h"

char ptx_code[] = "\n\
  .version 1.3\n\
  .target sm_11\n\
  .entry add2Kernel\n\
  (\n\
  .param        .u64 x_array,\n\
  .param        .s32 lenx,\n\
  .param        .u64 shapex,\n\
  .param        .u64 y_array,\n\
  .param        .s32 leny,\n\
  .param        .u64 shapey,\n\
  .param        .u64 output_array\n\
  )\n\
  {\n\
  \n\
  .reg          .u16 %rh<5>;\n\
  .reg          .u32 %r<13>;\n\
  .reg          .u64 %rd<10>;\n\
  .reg          .pred %p<3>;\n\
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
  ld.param.s32  %r8, [lenx];\n\
  setp.le.s32   %p1, %r8, %r7;\n\
  @%p1 bra      $Out_of_bounds;\n\
  \n\
  cvt.u64.s32   %rd1, %r7;\n\
  mul.lo.u64    %rd2, %rd1, 4;\n\
  ld.param.u64  %rd3, [x_array];\n\
  add.u64       %rd4, %rd3, %rd2;\n\
  ld.global.s32 %r9, [%rd4];\n\
  ld.param.u64  %rd5, [y_array];\n\
  add.u64       %rd6, %rd5, %rd2;\n\
  ld.global.s32 %r10, [%rd6];\n\
  \n\
  add.s32       %r11, %r9, %r10;\n\
  \n\
  ld.param.u64  %rd7, [output_array];\n\
  add.u64       %rd8, %rd7, %rd2;\n\
  st.global.s32 [%rd8], %r11;\n\
  \n\
  $Out_of_bounds:\n\
  exit;\n\
  }\n\
";

char test_k[] = "\n\
 .version 1.3\n\
 .target sm_11\n\
 .entry add2Kernel\n\
 (.param .u64 param1,\n\
  .param .s32 len1,\n\
  .param .u64 shape1,\n\
  .param .u64 param2,\n\
  .param .s32 len2,\n\
  .param .u64 shape2,\n\
  .param .u64 param3,\n\
  .param .s32 len3,\n\
  .param .u64 shape3)\n\
 {\n\
 .reg .u64 %rx1;\n\
 .reg .u32 %rlen1;\n\
 .reg .u64 %rshape1;\n\
 .reg .u64 %rx2;\n\
 .reg .u32 %rlen2;\n\
 .reg .u64 %rshape2;\n\
 .reg .u64 %rx3;\n\
 .reg .u32 %rlen3;\n\
 .reg .u64 %rshape3;\n\
 .reg .u16 %rh3;\n\
 .reg .u16 %rh2;\n\
 .reg .u16 %rh1;\n\
 .reg .u32 %r7;\n\
 .reg .u32 %r6;\n\
 .reg .u32 %r5;\n\
 .reg .u32 %r4;\n\
 .reg .u32 %r3;\n\
 .reg .u32 %r2;\n\
 .reg .u32 %r1;\n\
 .reg .pred %p1;\n\
 .reg .u32 %r8;\n\
 .reg .u32 %r9;\n\
 .reg .u32 %r10;\n\
 .reg .u64 %rl3;\n\
 .reg .u64 %rl2;\n\
 .reg .u64 %rl1;\n\
 .reg .s32 %rs1;\n\
 .reg .u64 %rl8;\n\
 .reg .u64 %rl7;\n\
 .reg .u64 %rl6;\n\
 .reg .u64 %rl5;\n\
 .reg .u64 %rl4;\n\
 .reg .s32 %rs2;\n\
 .reg .s32 %rs3;\n\
 .reg .u64 %rl9;\n\
\n\
 /* Loading arguments into registers */\n\
 ld.param.u64 %rx1, [param1];\n\
 ld.param.s32 %rlen1, [len1];\n\
 ld.param.u64 %rshape1, [shape1];\n\
 ld.param.u64 %rx2, [param2];\n\
 ld.param.s32 %rlen2, [len2];\n\
 ld.param.u64 %rshape2, [shape2];\n\
 ld.param.u64 %rx3, [param3];\n\
 ld.param.s32 %rlen3, [len3];\n\
 ld.param.u64 %rshape3, [shape3];\n\
\n\
 /* Computing thread ID */\n\
 mov.u16        %rh1, %ntid.x;\n\
 mov.u16        %rh2, %ctaid.x;\n\
 mul.wide.u16   %r1, %rh2, %rh1;\n\
 mov.u16 %rh3,  %nctaid.x;\n\
 mul.wide.u16   %r2, %rh3, %rh1;\n\
 cvt.u32.u16    %r3, %ctaid.y;\n\
 mul.lo.u32     %r4, %r3, %r2;\n\
 add.u32        %r5, %r1, %r4;\n\
 cvt.u32.u16    %r6, %tid.x;\n\
 add.u32        %r7, %r6, %r5;\n\
 \n\
 setp.le.u32    %p1, %rlen1, %r7;\n\
 @%p1 bra $Out_of_bounds ;\n\
 \n\
 /* Loading shape of largest argument into registers */\n\
 ld.global.u32 %r8, [%rshape1];\n\
 \n\
 /* Loading from array */\n\
 cvt.u64.u32    %rl1, %r7;\n\
 mul.lo.u64     %rl2, %rl1, 4;\n\
 add.u64        %rl3, %rx1, %rl2;\n\
 ld.global.s32  %rs1, [%rl3];\n\
 \n\
 /* Loading from array */\n\
 cvt.u64.u32    %rl4, %r7;\n\
 mul.lo.u64     %rl4, %rl4, 4;\n\
 add.u64        %rl6, %rx2, %rl4;\n\
 ld.global.s32  %rs2, [%rl6];\n\
 \n\
 /* Generating body */\n\
 add.s32        %rs3, %rs1, %rs2;\n\
 /* Storing into array */\n\
 \n\
 cvt.u64.u32    %rl7, %r7;\n\
 mul.lo.u64     %rl8, %rl7, 4;\n\
 add.u64        %rl9, %rx3, %rl8;\n\
 st.global.s32 [%rl9], %rs3;\n\
 \n\
 $Out_of_bounds:\n\
 exit ;\n\
 }\n\
";

char output_k[] = "\n\
.version 1.3\n\
 .target sm_11\n\
.entry add2Kernel\n\
(.param .u64 param1, .param .u32 len1, .param .u64 shape1, .param .u64 param2, .param .u32 len2, .param .u64 shape2, .param .u64 param3, .param .u32 len3, .param .u64 shape3)\n\
{\n\
.reg    .u64 %rx1;\n\
.reg    .u32 %rlen1;\n\
.reg    .u64 %rshape1;\n\
.reg    .u64 %rx2;\n\
.reg    .u32 %rlen2;\n\
.reg    .u64 %rshape2;\n\
.reg    .u64 %rx3;\n\
.reg    .u32 %rlen3;\n\
.reg    .u64 %rshape3;\n\
.reg    .u16 %rh3;\n\
.reg    .u16 %rh2;\n\
.reg    .u16 %rh1;\n\
.reg    .u32 %r7;\n\
.reg    .u32 %r6;\n\
.reg    .u32 %r5;\n\
.reg    .u32 %r4;\n\
.reg    .u32 %r3;\n\
.reg    .u32 %r2;\n\
.reg    .u32 %r1;\n\
.reg    .pred %p1;\n\
.reg    .u32 %r8;\n\
.reg    .u32 %r9;\n\
.reg    .u32 %r10;\n\
.reg    .u64 %rl3;\n\
.reg    .u64 %rl2;\n\
.reg    .u64 %rl1;\n\
.reg    .s32 %rs1;\n\
.reg    .u64 %rl6;\n\
.reg    .u64 %rl5;\n\
.reg    .u64 %rl4;\n\
.reg    .s32 %rs2;\n\
.reg    .s32 %rs3;\n\
.reg    .u64 %rl9;\n\
.reg    .u64 %rl8;\n\
.reg    .u64 %rl7;\n\
\n\
/* Loading arguments into registers */\n\
ld.param.u64 %rx1, [param1];\n\
ld.param.u32 %rlen1, [len1];\n\
ld.param.u64 %rshape1, [shape1];\n\
ld.param.u64 %rx2, [param2];\n\
ld.param.u32 %rlen2, [len2];\n\
ld.param.u64 %rshape2, [shape2];\n\
ld.param.u64 %rx3, [param3];\n\
ld.param.u32 %rlen3, [len3];\n\
ld.param.u64 %rshape3, [shape3];\n\
/* Computing thread ID */\n\
mov.u16 %rh1, %ntid.x;\n\
mov.u16 %rh2, %ctaid.x;\n\
mul.wide.u16 %r1, %rh2, %rh1;\n\
mov.u16 %rh3, %nctaid.x;\n\
mul.wide.u16 %r2, %rh3, %rh1;\n\
cvt.u32.u16 %r3, %ctaid.y;\n\
mul.lo.u32 %r4, %r3, %r2;\n\
add.u32 %r5, %r1, %r4;\n\
cvt.u32.u16 %r6, %tid.x;\n\
add.u32 %r7, %r6, %r5;\n\
setp.le.u32 %p1, %rlen1, %r7;\n\
@%p1 bra $Out_of_bounds ;\n\
/* Storing into array */\n\
cvt.u64.u32 %rl7, %r7;\n\
mul.lo.u64 %rl8, %rl7, 4;\n\
add.u64 %rl9, %rx3, %rl8;\n\
st.global.s32 [%rl9], 0;\n\
$Out_of_bounds:\n\
exit ;\n\
}\n\
";

int main(int argc, char **argv) {
  int ar_size = 1000;

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

  /* Actually do the Cuda stuff */
  pq_init();

  CUcontext ctx = 0;
  CUresult rslt = cuCtxCreate(&ctx, 0, 0);
  printf("created context with rslt %d\n", rslt);

  CUdeviceptr cuda_x_array;
  rslt = cuMemAlloc(&cuda_x_array, mem_size);
  printf("alloc x array with rslt %d\n", rslt);
  CUdeviceptr cuda_y_array;
  rslt = cuMemAlloc(&cuda_y_array, mem_size);
  printf("alloc y array with rslt %d\n", rslt);

  printf("Cuda memory alloc'ed.\n");

  rslt = cuMemcpyHtoD(cuda_x_array, x_array, mem_size);
  printf("copy x array with rslt %d\n", rslt);
  rslt = cuMemcpyHtoD(cuda_y_array, y_array, mem_size);
  printf("copy y array with rslt %d\n", rslt);

  printf("Arrays copied to device.\n");

  // Read in the file of ptx
  FILE * file;
  long size;
  char *buffer;
  size_t result;
  file = fopen("add.ptx", "r");
  if (file == NULL) {
    printf("Unable to open ptx file.\n");
    exit(-1);
  }
  fseek(file, 0, SEEK_END);
  size = ftell(file);
  rewind(file);
  buffer = (char*)malloc(sizeof(char)*size);
  result = fread(buffer, 1, size, file);
  if (result != size) {
    printf("Unable to read in file.\n");
    exit(-1);
  }
  fclose(file);

  CUmodule *cuModule = pq_compile_module(buffer);
  free(buffer);

  printf("Ptx compiled.\n");

  int shape[1];
  shape[0] = ar_size;
  CUdeviceptr cuda_shape;
  cuMemAlloc(&cuda_shape, sizeof(int));
  cuMemcpyHtoD(cuda_shape, shape, sizeof(int));

  void *kernel_args[9];
  kernel_args[0] = (void*)cuda_x_array;
  kernel_args[1] = &ar_size;
  kernel_args[2] = (void*)cuda_shape;
  kernel_args[3] = (void*)cuda_y_array;
  kernel_args[4] = &ar_size;
  kernel_args[5] = (void*)cuda_shape;
  kernel_args[6] = (void*)cuda_x_array;
  kernel_args[7] = &ar_size;
  kernel_args[8] = (void*)cuda_shape;
  printf("cuda_x_array: %x\n", cuda_x_array);
  printf("kernel_args[0]: %p\n", kernel_args[0]);

  int sizes[9];
  sizes[0] = sizes[2] = sizes[3] = sizes[5] = sizes[6] =
             sizes[8] = sizeof(void*);
  sizes[1] = sizes[4] = sizes[7] = sizeof(int);
  int aligns[9];
  aligns[0] = aligns[2] = aligns[3] = aligns[5] = aligns[6] = aligns[8] = 1;
  aligns[1] = aligns[4] = aligns[7] = 0;
  printf("sizeof(CUdeviceptr): %d\n", sizeof(CUdeviceptr));
  printf("sizeof(int) : %d\n", sizeof(int));
  printf("alignof(int): %d\n", __alignof(int));

  pq_launch_ptx(cuModule, "add2Kernel", kernel_args, 9, aligns, sizes,
                256, 1, 1, ar_size / 256 + 1, 1);

  printf("Kernel launched.\n");

  rslt = cuMemcpyDtoH(out_array, cuda_x_array, mem_size);

  printf("Output array copied to host %d.\n", rslt);

  int same = 1;
  for (i = 0; i < ar_size; ++i) {
    if (i < 10) {
      printf("x_array[%d]: %d\n", i, x_array[i]);
      printf("y_array[%d]: %d\n", i, y_array[i]);
      printf("out_array[%d]: %d\n", i, out_array[i]);
    }
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

  return 0;
}
