/*
 * Parallel Q Library
 *
 * (c) 2009-2010 Eric Hielscher
 *
 * Reduction.
 */

#include <cuda.h>
#include <stdio.h>

#define THREADS_PER_BLOCK 128

/*
 * Assumes that the size of the output array is equal to the number of thread
 * blocks.
 */

int safe_div(int n, int d) {
  return (n + d - 1) / d;
}

/*
__global__ void
reduce_float_kernel(float *input, float *output, unsigned int num_input)
{
    __shared__ float cache[THREADS_PER_BLOCK];

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid      = threadIdx.x;
    unsigned int i        = blockIdx.x*(THREADS_PER_BLOCK*2) + threadIdx.x;
    unsigned int gridSize = THREADS_PER_BLOCK*2*gridDim.x;
    cache[tid] = 0;

    // we reduce multiple elements per thread.  The number is determined by the 
    // number of active thread blocks (via gridDim).  More blocks will result
    // in a larger gridSize and therefore fewer elements per thread
    while (i < num_input)
    {         
        cache[tid] += input[i];
        // ensure we don't read out of bounds
        // Note: Can remove the if when n is a power of 2
        if (i + THREADS_PER_BLOCK < num_input) 
            cache[tid] += input[i + THREADS_PER_BLOCK];  
        i += gridSize;
    } 
    __syncthreads();

    // do reduction in shared mem
    if (tid < 128) { cache[tid] += cache[tid + 128]; } __syncthreads();
    if (tid <  64) { cache[tid] += cache[tid +  64]; } __syncthreads();
    
    if (tid < 32)
    {
        cache[tid] += cache[tid + 32];
        cache[tid] += cache[tid + 16];
        cache[tid] += cache[tid +  8];
        cache[tid] += cache[tid +  4];
        cache[tid] += cache[tid +  2];
        cache[tid] += cache[tid +  1];
    }
    
    // write result for this block to global mem 
    if (tid == 0) output[blockIdx.x] = cache[0];
}

__global__ void
reduce_int_kernel(int *input, int *output, unsigned int num_input)
{
    __shared__ int cache[THREADS_PER_BLOCK];

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid      = threadIdx.x;
    unsigned int i        = blockIdx.x*(THREADS_PER_BLOCK*2) + threadIdx.x;
    unsigned int gridSize = THREADS_PER_BLOCK*2*gridDim.x;
    cache[tid] = 0;

    // we reduce multiple elements per thread.  The number is determined by the 
    // number of active thread blocks (via gridDim).  More blocks will result
    // in a larger gridSize and therefore fewer elements per thread
    while (i < num_input)
    {         
        cache[tid] += input[i];
        // ensure we don't read out of bounds
        // Note: Can remove the if when n is a power of 2
        if (i + THREADS_PER_BLOCK < num_input) 
            cache[tid] += input[i + THREADS_PER_BLOCK];  
        i += gridSize;
    } 
    __syncthreads();

    // do reduction in shared mem
    if (tid < 128) { cache[tid] += cache[tid + 128]; } __syncthreads();
    if (tid <  64) { cache[tid] += cache[tid +  64]; } __syncthreads();
    
    if (tid < 32)
    {
        cache[tid] += cache[tid + 32];
        cache[tid] += cache[tid + 16];
        cache[tid] += cache[tid +  8];
        cache[tid] += cache[tid +  4];
        cache[tid] += cache[tid +  2];
        cache[tid] += cache[tid +  1];
    }
    
    // write result for this block to global mem 
    if (tid == 0) output[blockIdx.x] = cache[0];
}
*/

char *ptx = "\
.version 1.4 \
.target sm_13 \
.entry reduce_kernel84 \
(.param .u64 __param0, .param .u64 shape66, .param .u64 __param1, .param .u64 shape70) \
{ \
.shared  .align 8 .b8 __shared0[1024]; \
.reg     .u64 %rl0; \
.reg     .u64 %rl1; \
.reg     .u64 %rl2;\
.reg     .u64 %rl3;\
.reg     .s16 %rsh5;\
.reg     .u16 %rh0;\
.reg     .s16 %rsh6;\
.reg     .s32 %rs44;\
.reg     .s16 %rsh7;\
.reg     .u16 %rh1;\
.reg     .s16 %rsh8;\
.reg     .u16 %rh2;\
.reg     .s32 %rs45;\
.reg     .s32 %rs46;\
.reg     .s32 %rs47;\
.reg     .s16 %rsh9;\
.reg     .u16 %rh3;\
.reg     .s32 %rs48;\
.reg     .s32 %rs49;\
.reg     .s32 %rs50;\
.reg     .s32 %rs51;\
.reg     .s32 %rs52;\
.reg     .s32 %rs53;\
.reg     .pred %p14;\
.reg     .s32 %rs54;\
.reg     .s32 %rs55;\
.reg     .pred %p15;\
.reg     .s32 %rs56;\
.reg     .s32 %rs57;\
.reg     .u64 %rl5;\
.reg     .u64 %rl6;\
.reg     .u64 %rl7;\
.reg     .s32 %rs58;\
.reg     .u64 %rl8;\
.reg     .u64 %rl9;\
.reg     .u64 %rl10;\
.reg     .s32 %rs59;\
.reg     .s32 %rs60;\
.reg     .s32 %rs61;\
.reg     .u64 %rl11;\
.reg     .u64 %rl12;\
.reg     .u64 %rl13;\
.reg     .u64 %rl14;\
.reg     .s32 %rs62;\
.reg     .s32 %rs63;\
.reg     .s32 %rs64;\
.reg     .s32 %rs65;\
.reg     .u64 %rl15;\
.reg     .u64 %rl16;\
.reg     .u64 %rl17;\
.reg     .u64 %rl18;\
.reg     .s32 %rs66;\
.reg     .s32 %rs67;\
.reg     .s32 %rs68;\
.reg     .u64 %rl19;\
.reg     .u64 %rl20;\
.reg     .u64 %rl21;\
.reg     .s32 %rs69;\
.reg     .s32 %rs70;\
.reg     .s32 %rs71;\
.reg     .pred %p16;\
.reg     .s32 %rs72;\
.reg     .s32 %rs73;\
.reg     .s32 %rs74;\
.reg     .pred %p17;\
.reg     .pred %p18;\
.reg     .pred %p19;\
.reg     .s32 %rs75;\
.reg     .s32 %rs76;\
.reg     .s32 %rs77;\
.reg     .u64 %rl22;\
.reg     .u64 %rl23;\
.reg     .u64 %rl24;\
.reg     .s32 %rs78;\
.reg     .s32 %rs79;\
.reg     .s32 %rs80;\
.reg     .s32 %rs81;\
.reg     .u64 %rl25;\
.reg     .u64 %rl26;\
.reg     .u64 %rl27;\
.reg     .s32 %rs82;\
.reg     .s32 %rs83;\
.reg     .s32 %rs84;\
.reg     .u64 %rl28;\
.reg     .u64 %rl29;\
.reg     .u64 %rl30;\
.reg     .s32 %rs85;\
.reg     .pred %p20;\
.reg     .pred %p21;\
.reg     .pred %p22;\
.reg     .s32 %rs86;\
.reg     .s32 %rs87;\
.reg     .s32 %rs88;\
.reg     .u64 %rl31;\
.reg     .u64 %rl32;\
.reg     .u64 %rl33;\
.reg     .s32 %rs89;\
.reg     .s32 %rs90;\
.reg     .s32 %rs91;\
.reg     .s32 %rs92;\
.reg     .u64 %rl34;\
.reg     .u64 %rl35;\
.reg     .u64 %rl36;\
.reg     .s32 %rs93;\
.reg     .s32 %rs94;\
.reg     .s32 %rs95;\
.reg     .u64 %rl37;\
.reg     .u64 %rl38;\
.reg     .u64 %rl39;\
.reg     .s32 %rs96;\
.reg     .pred %p23;\
.reg     .pred %p24;\
.reg     .pred %p25;\
.reg     .s32 %rs97;\
.reg     .s32 %rs98;\
.reg     .s32 %rs99;\
.reg     .u64 %rl40;\
.reg     .u64 %rl41;\
.reg     .u64 %rl42;\
.reg     .s32 %rs100;\
.reg     .s32 %rs101;\
.reg     .s32 %rs102;\
.reg     .s32 %rs103;\
.reg     .u64 %rl43;\
.reg     .u64 %rl44;\
.reg     .u64 %rl45;\
.reg     .s32 %rs104;\
.reg     .s32 %rs105;\
.reg     .s32 %rs106;\
.reg     .u64 %rl46;\
.reg     .u64 %rl47;\
.reg     .u64 %rl48;\
.reg     .s32 %rs107;\
.reg     .pred %p26;\
.reg     .pred %p27;\
.reg     .pred %p28;\
.reg     .s32 %rs108;\
.reg     .s32 %rs109;\
.reg     .s32 %rs110;\
.reg     .u64 %rl49;\
.reg     .u64 %rl50;\
.reg     .u64 %rl51;\
.reg     .s32 %rs111;\
.reg     .s32 %rs112;\
.reg     .s32 %rs113;\
.reg     .s32 %rs114;\
.reg     .u64 %rl52;\
.reg     .u64 %rl53;\
.reg     .u64 %rl54;\
.reg     .s32 %rs115;\
.reg     .s32 %rs116;\
.reg     .s32 %rs117;\
.reg     .u64 %rl55;\
.reg     .u64 %rl56;\
.reg     .u64 %rl57;\
.reg     .s32 %rs118;\
.reg     .pred %p29;\
.reg     .pred %p30;\
.reg     .pred %p31;\
.reg     .s32 %rs119;\
.reg     .s32 %rs120;\
.reg     .s32 %rs121;\
.reg     .u64 %rl58;\
.reg     .u64 %rl59;\
.reg     .u64 %rl60;\
.reg     .s32 %rs122;\
.reg     .s32 %rs123;\
.reg     .s32 %rs124;\
.reg     .s32 %rs125;\
.reg     .u64 %rl61;\
.reg     .u64 %rl62;\
.reg     .u64 %rl63;\
.reg     .s32 %rs126;\
.reg     .s32 %rs127;\
.reg     .s32 %rs128;\
.reg     .u64 %rl64;\
.reg     .u64 %rl65;\
.reg     .u64 %rl66;\
.reg     .s32 %rs129;\
.reg     .pred %p32;\
.reg     .pred %p33;\
.reg     .pred %p34;\
.reg     .s32 %rs130;\
.reg     .s32 %rs131;\
.reg     .s32 %rs132;\
.reg     .u64 %rl67;\
.reg     .u64 %rl68;\
.reg     .u64 %rl69;\
.reg     .s32 %rs133;\
.reg     .s32 %rs134;\
.reg     .s32 %rs135;\
.reg     .s32 %rs136;\
.reg     .u64 %rl70;\
.reg     .u64 %rl71;\
.reg     .u64 %rl72;\
.reg     .s32 %rs137;\
.reg     .s32 %rs138;\
.reg     .s32 %rs139;\
.reg     .u64 %rl73;\
.reg     .u64 %rl74;\
.reg     .u64 %rl75;\
.reg     .s32 %rs140;\
.reg     .pred %p35;\
.reg     .pred %p36;\
.reg     .pred %p37;\
.reg     .s32 %rs141;\
.reg     .s32 %rs142;\
.reg     .s32 %rs143;\
.reg     .u64 %rl76;\
.reg     .u64 %rl77;\
.reg     .u64 %rl78;\
.reg     .s32 %rs144;\
.reg     .s32 %rs145;\
.reg     .s32 %rs146;\
.reg     .s32 %rs147;\
.reg     .u64 %rl79;\
.reg     .u64 %rl80;\
.reg     .u64 %rl81;\
.reg     .s32 %rs148;\
.reg     .s32 %rs149;\
.reg     .s32 %rs150;\
.reg     .u64 %rl82;\
.reg     .u64 %rl83;\
.reg     .u64 %rl84;\
.reg     .pred %p38;\
.reg     .s32 %rs151;\
.reg     .s32 %rs152;\
.reg     .u64 %rl85;\
.reg     .u64 %rl86;\
.reg     .u64 %rl87;\
.reg     .s32 %rs153;\
.reg     .u64 %rl88;\
.reg     .u64 %rl89;\
.reg     .u64 %rl90;\
\
mov.u64  %rl0, __shared0;\
ld.param.u64     %rl1, [__param0];\
ld.param.u64     %rl2, [shape66];\
ld.param.u64     %rl3, [__param1];\
mov.u16  %rh0, %tid.x;\
cvt.s16.u16      %rsh5, %rh0;\
mov.s16  %rsh6, %rsh5;\
/* Conversion from int16 to int32  */\
cvt.s32.s16      %rs44, %rsh6;\
mov.u16  %rh1, %nctaid.x;\
cvt.s16.u16      %rsh7, %rh1;\
mov.u16  %rh2, %ctaid.y;\
cvt.s16.u16      %rsh8, %rh2;\
cvt.s32.s16      %rs46, %rsh8;\
cvt.s32.s16      %rs47, %rsh7;\
mul.lo.s32       %rs45, %rs46, %rs47;\
mov.u16  %rh3, %ctaid.x;\
cvt.s16.u16      %rsh9, %rh3;\
cvt.s32.s16      %rs49, %rsh9;\
add.s32  %rs48, %rs49, %rs45;\
mul.lo.s32       %rs50, 256, %rs48;\
cvt.s32.s16      %rs52, %rsh5;\
add.s32  %rs51, %rs52, %rs50;\
ld.global.u32    %rs53, [%rl2];\
setp.lt.s32      %p14, %rs51, %rs53;\
@%p14 bra $Label2;\
bra $Label1;\
$Label2:\
/* true branch */\
ld.global.u32    %rs54, [%rl2];\
add.s32  %rs55, %rs51, 128;\
setp.lt.s32      %p15, %rs55, %rs54;\
@%p15 bra $Label4;\
/* array store  */\
mov.s32  %rs56, %rs44;\
cvt.u64.u32      %rl5, %rs51;\
mul.lo.u64       %rl6, %rl5, 4;\
add.u64  %rl7, %rl1, %rl6;\
ld.global.s32    %rs58, [%rl7];\
mov.s32  %rs57, %rs58;\
cvt.u64.s32      %rl8, %rs56;\
mul.lo.u64       %rl9, %rl8, 4;\
add.u64  %rl10, %rl0, %rl9;\
st.shared.s32    [%rl10], %rs57;\
bra $Label3;\
$Label4:\
mov.s32  %rs59, %rs51;\
mov.u64  %rl11, 4;\
cvt.u64.s32      %rl12, %rs59;\
mul.lo.u64       %rl13, %rl11, %rl12;\
add.u64  %rl14, %rl1, %rl13;\
ld.global.s32    %rs61, [%rl14];\
mov.s32  %rs60, %rs61;\
add.s32  %rs62, %rs51, 128;\
mov.s32  %rs63, %rs62;\
mov.u64  %rl15, 4;\
cvt.u64.s32      %rl16, %rs63;\
mul.lo.u64       %rl17, %rl15, %rl16;\
add.u64  %rl18, %rl1, %rl17;\
ld.global.s32    %rs65, [%rl18];\
mov.s32  %rs64, %rs65;\
add.s32  %rs66, %rs60, %rs64;\
mov.s32  %rs67, %rs44;\
mov.s32  %rs68, %rs66;\
cvt.u64.s32      %rl19, %rs67;\
mul.lo.u64       %rl20, %rl19, 4;\
add.u64  %rl21, %rl0, %rl20;\
st.shared.s32    [%rl21], %rs68;\
$Label3:\
$Label1:\
bar.sync 0;\
mov.s32  %rs69, 256;\
ld.global.u32    %rs70, [%rl2];\
sub.s32  %rs71, %rs70, %rs51;\
setp.lt.s32      %p16, %rs71, %rs69;\
@%p16 bra $Label6;\
bra $Label5;\
$Label6:\
/* true branch */\
ld.global.u32    %rs72, [%rl2];\
sub.s32  %rs73, %rs72, %rs50;\
$Label5:\
/* branches of if-stmt converge */\
add.s32  %rs74, %rs44, 64;\
setp.lt.s32      %p17, %rs74, %rs73;\
setp.lt.s32      %p18, %rs44, 64;\
and.pred         %p19, %p18, %p17;\
@%p19 bra $Label8;\
bra $Label7;\
$Label8:\
/* true branch */\
mov.s32  %rs75, %rs44;\
/* generating array load */\
cvt.u64.s32      %rl24, %rs75;\
mul.lo.u64       %rl23, %rl24, 4;\
add.u64  %rl22, %rl0, %rl23;\
ld.shared.s32    %rs77, [%rl22];\
mov.s32  %rs76, %rs77;\
add.s32  %rs78, %rs44, 64;\
mov.s32  %rs79, %rs78;\
/* generating array load */\
cvt.u64.s32      %rl27, %rs79;\
mul.lo.u64       %rl26, %rl27, 4;\
add.u64  %rl25, %rl0, %rl26;\
ld.shared.s32    %rs81, [%rl25];\
mov.s32  %rs80, %rs81;\
add.s32  %rs82, %rs80, %rs76;\
/* array store  */\
mov.s32  %rs83, %rs44;\
mov.s32  %rs84, %rs82;\
/* calculating storage index */\
cvt.u64.s32      %rl28, %rs83;\
mul.lo.u64       %rl29, %rl28, 4;\
add.u64  %rl30, %rl0, %rl29;\
st.shared.s32    [%rl30], %rs84;\
$Label7:\
/* branches of if-stmt converge */\
bar.sync 0;\
add.s32  %rs85, %rs44, 32;\
setp.lt.s32      %p20, %rs85, %rs73;\
setp.lt.s32      %p21, %rs44, 32;\
and.pred         %p22, %p21, %p20;\
@%p22 bra $Label10;\
bra $Label9;\
$Label10:\
/* true branch */\
mov.s32  %rs86, %rs44;\
/* generating array load */\
cvt.u64.s32      %rl33, %rs86;\
mul.lo.u64       %rl32, %rl33, 4;\
add.u64  %rl31, %rl0, %rl32;\
ld.shared.s32    %rs88, [%rl31];\
mov.s32  %rs87, %rs88;\
add.s32  %rs89, %rs44, 32;\
mov.s32  %rs90, %rs89;\
/* generating array load */\
cvt.u64.s32      %rl36, %rs90;\
mul.lo.u64       %rl35, %rl36, 4;\
add.u64  %rl34, %rl0, %rl35;\
ld.shared.s32    %rs92, [%rl34];\
mov.s32  %rs91, %rs92;\
add.s32  %rs93, %rs91, %rs87;\
/* array store  */\
mov.s32  %rs94, %rs44;\
mov.s32  %rs95, %rs93;\
/* calculating storage index */\
cvt.u64.s32      %rl37, %rs94;\
mul.lo.u64       %rl38, %rl37, 4;\
add.u64  %rl39, %rl0, %rl38;\
st.shared.s32    [%rl39], %rs95;\
$Label9:\
/* branches of if-stmt converge */\
bar.sync 0;\
add.s32  %rs96, %rs44, 16;\
setp.lt.s32      %p23, %rs96, %rs73;\
setp.lt.s32      %p24, %rs44, 16;\
and.pred         %p25, %p24, %p23;\
@%p25 bra $Label12;\
bra $Label11;\
$Label12:\
/* true branch */\
mov.s32  %rs97, %rs44;\
/* generating array load */\
cvt.u64.s32      %rl42, %rs97;\
mul.lo.u64       %rl41, %rl42, 4;\
add.u64  %rl40, %rl0, %rl41;\
ld.shared.s32    %rs99, [%rl40];\
mov.s32  %rs98, %rs99;\
add.s32  %rs100, %rs44, 16;\
mov.s32  %rs101, %rs100;\
/* generating array load */\
cvt.u64.s32      %rl45, %rs101;\
mul.lo.u64       %rl44, %rl45, 4;\
add.u64  %rl43, %rl0, %rl44;\
ld.shared.s32    %rs103, [%rl43];\
mov.s32  %rs102, %rs103;\
add.s32  %rs104, %rs102, %rs98;\
/* array store  */\
mov.s32  %rs105, %rs44;\
mov.s32  %rs106, %rs104;\
/* calculating storage index */\
cvt.u64.s32      %rl46, %rs105;\
mul.lo.u64       %rl47, %rl46, 4;\
add.u64  %rl48, %rl0, %rl47;\
st.shared.s32    [%rl48], %rs106;\
$Label11:\
/* branches of if-stmt converge */\
bar.sync 0;\
add.s32  %rs107, %rs44, 8;\
setp.lt.s32      %p26, %rs107, %rs73;\
setp.lt.s32      %p27, %rs44, 8;\
and.pred         %p28, %p27, %p26;\
@%p28 bra $Label14;\
bra $Label13;\
$Label14:\
/* true branch */\
mov.s32  %rs108, %rs44;\
/* generating array load */\
cvt.u64.s32      %rl51, %rs108;\
mul.lo.u64       %rl50, %rl51, 4;\
add.u64  %rl49, %rl0, %rl50;\
ld.shared.s32    %rs110, [%rl49];\
mov.s32  %rs109, %rs110;\
add.s32  %rs111, %rs44, 8;\
mov.s32  %rs112, %rs111;\
/* generating array load */\
cvt.u64.s32      %rl54, %rs112;\
mul.lo.u64       %rl53, %rl54, 4;\
add.u64  %rl52, %rl0, %rl53;\
ld.shared.s32    %rs114, [%rl52];\
mov.s32  %rs113, %rs114;\
add.s32  %rs115, %rs113, %rs109;\
/* array store  */\
mov.s32  %rs116, %rs44;\
mov.s32  %rs117, %rs115;\
/* calculating storage index */\
cvt.u64.s32      %rl55, %rs116;\
mul.lo.u64       %rl56, %rl55, 4;\
add.u64  %rl57, %rl0, %rl56;\
st.shared.s32    [%rl57], %rs117;\
$Label13:\
/* branches of if-stmt converge */\
bar.sync 0;\
add.s32  %rs118, %rs44, 4;\
setp.lt.s32      %p29, %rs118, %rs73;\
setp.lt.s32      %p30, %rs44, 4;\
and.pred         %p31, %p30, %p29;\
@%p31 bra $Label16;\
bra $Label15;\
$Label16:\
/* true branch */\
mov.s32  %rs119, %rs44;\
/* generating array load */\
cvt.u64.s32      %rl60, %rs119;\
mul.lo.u64       %rl59, %rl60, 4;\
add.u64  %rl58, %rl0, %rl59;\
ld.shared.s32    %rs121, [%rl58];\
mov.s32  %rs120, %rs121;\
add.s32  %rs122, %rs44, 4;\
mov.s32  %rs123, %rs122;\
/* generating array load */\
cvt.u64.s32      %rl63, %rs123;\
mul.lo.u64       %rl62, %rl63, 4;\
add.u64  %rl61, %rl0, %rl62;\
ld.shared.s32    %rs125, [%rl61];\
mov.s32  %rs124, %rs125;\
add.s32  %rs126, %rs124, %rs120;\
/* array store  */\
mov.s32  %rs127, %rs44;\
mov.s32  %rs128, %rs126;\
/* calculating storage index */\
cvt.u64.s32      %rl64, %rs127;\
mul.lo.u64       %rl65, %rl64, 4;\
add.u64  %rl66, %rl0, %rl65;\
st.shared.s32    [%rl66], %rs128;\
$Label15:\
/* branches of if-stmt converge */\
bar.sync 0;\
add.s32  %rs129, %rs44, 2;\
setp.lt.s32      %p32, %rs129, %rs73;\
setp.lt.s32      %p33, %rs44, 2;\
and.pred         %p34, %p33, %p32;\
@%p34 bra $Label18;\
bra $Label17;\
$Label18:\
/* true branch */\
mov.s32  %rs130, %rs44;\
/* generating array load */\
cvt.u64.s32      %rl69, %rs130;\
mul.lo.u64       %rl68, %rl69, 4;\
add.u64  %rl67, %rl0, %rl68;\
ld.shared.s32    %rs132, [%rl67];\
mov.s32  %rs131, %rs132;\
add.s32  %rs133, %rs44, 2;\
mov.s32  %rs134, %rs133;\
/* generating array load */\
cvt.u64.s32      %rl72, %rs134;\
mul.lo.u64       %rl71, %rl72, 4;\
add.u64  %rl70, %rl0, %rl71;\
ld.shared.s32    %rs136, [%rl70];\
mov.s32  %rs135, %rs136;\
add.s32  %rs137, %rs135, %rs131;\
/* array store  */\
mov.s32  %rs138, %rs44;\
mov.s32  %rs139, %rs137;\
/* calculating storage index */\
cvt.u64.s32      %rl73, %rs138;\
mul.lo.u64       %rl74, %rl73, 4;\
add.u64  %rl75, %rl0, %rl74;\
st.shared.s32    [%rl75], %rs139;\
$Label17:\
/* branches of if-stmt converge */\
bar.sync 0;\
add.s32  %rs140, %rs44, 1;\
setp.lt.s32      %p35, %rs140, %rs73;\
setp.lt.s32      %p36, %rs44, 1;\
and.pred         %p37, %p36, %p35;\
@%p37 bra $Label20;\
bra $Label19;\
$Label20:\
/* true branch */\
mov.s32  %rs141, %rs44;\
/* generating array load */\
cvt.u64.s32      %rl78, %rs141;\
mul.lo.u64       %rl77, %rl78, 4;\
add.u64  %rl76, %rl0, %rl77;\
ld.shared.s32    %rs143, [%rl76];\
mov.s32  %rs142, %rs143;\
add.s32  %rs144, %rs44, 1;\
mov.s32  %rs145, %rs144;\
/* generating array load */\
cvt.u64.s32      %rl81, %rs145;\
mul.lo.u64       %rl80, %rl81, 4;\
add.u64  %rl79, %rl0, %rl80;\
ld.shared.s32    %rs147, [%rl79];\
mov.s32  %rs146, %rs147;\
add.s32  %rs148, %rs146, %rs142;\
/* array store  */\
mov.s32  %rs149, %rs44;\
mov.s32  %rs150, %rs148;\
/* calculating storage index */\
cvt.u64.s32      %rl82, %rs149;\
mul.lo.u64       %rl83, %rl82, 4;\
add.u64  %rl84, %rl0, %rl83;\
st.shared.s32    [%rl84], %rs150;\
$Label19:\
/* branches of if-stmt converge */\
bar.sync 0;\
setp.eq.s32      %p38, %rs44, 0;\
@%p38 bra $Label22;\
bra $Label21;\
$Label22:\
/* true branch */\
/* array store  */\
mov.s32  %rs151, %rs48;\
/* Calculating load index */\
cvt.u64.u32      %rl85, 0;\
mul.lo.u64       %rl86, %rl85, 4;\
add.u64  %rl87, %rl0, %rl86;\
ld.shared.s32    %rs153, [%rl87];\
mov.s32  %rs152, %rs153;\
/* calculating storage index */\
cvt.u64.s32      %rl88, %rs151;\
mul.lo.u64       %rl89, %rl88, 4;\
add.u64  %rl90, %rl3, %rl89;\
st.global.s32    [%rl90], %rs152;\
$Label21:\
/* branches of if-stmt converge */\
}\
"


__global__ void
reduce_int_kernel_nonopt(int *input, int *output, unsigned int num_input)
{
    __shared__ int cache[THREADS_PER_BLOCK];

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid            = threadIdx.x;
    unsigned int linearBlockIdx = blockIdx.x + (blockIdx.y * gridDim.x);
    unsigned int startBlock     = THREADS_PER_BLOCK * 2 * linearBlockIdx;
    unsigned int i              = startBlock + threadIdx.x;

    if (i < num_input) {
      if ((i + THREADS_PER_BLOCK) < num_input) {
        int tmp1 = input[i];
        int tmp2 = input[i + THREADS_PER_BLOCK];
        cache[tid] = tmp1 + tmp2;
      } else {
        cache[tid] = input[i];
      }
    }
    __syncthreads();

    unsigned int lenBlock = THREADS_PER_BLOCK * 2;
    if ((num_input - startBlock) < lenBlock) {
      lenBlock = num_input - startBlock;
    }

    // do reduction in shared mem
    if (tid < 64 && (tid + 64) < lenBlock) {
      cache[tid] += cache[tid + 64];
    } __syncthreads();
    if (tid < 32 && (tid + 32) < lenBlock) {
      cache[tid] += cache[tid + 32];
    } __syncthreads();
    if (tid < 16 && (tid + 16) < lenBlock) {
      cache[tid] += cache[tid + 16];
    } __syncthreads();
    if (tid <  8 && (tid +  8) < lenBlock) {
      cache[tid] += cache[tid +  8];
    } __syncthreads();
    if (tid <  4 && (tid +  4) < lenBlock) {
      cache[tid] += cache[tid +  4];
    } __syncthreads();
    if (tid <  2 && (tid +  2) < lenBlock) {
      cache[tid] += cache[tid +  2];
    } __syncthreads();
    if (tid == 0 && lenBlock > 1) {
      output[linearBlockIdx] = cache[0] + cache[1];
    }
}

#ifdef __cplusplus
extern "C" {
#endif

void reduce(int *input, int* output, int num_input) {
  // Call min-all
  cudaError_t rslt;
  cudaEvent_t start, stop;
  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  CUmodule *cuModule = pq_compile_module(ptx);

  // Alloc device copies
  int *devInput;
  int *devOutput;
  cudaEventRecord(start, 0);
  rslt = cudaMalloc((void**)&devInput, num_input * sizeof(int));
  if (rslt != 0) printf("failed to malloc dev Input: %d\n", rslt);
  // Copy data to device
  rslt = cudaMemcpy(devInput, input, num_input * sizeof(int),
                    cudaMemcpyHostToDevice);
  if (rslt != 0) printf("failed to copy input: %d\n", rslt);
  int curNum = num_input;
  while (curNum > 1) {
    int numBlocks = safe_div(curNum, THREADS_PER_BLOCK * 2);
    rslt = cudaMalloc((void**)&devOutput, numBlocks * sizeof(int));
    if (rslt != 0) printf("failed to malloc dev Output: %d\n", rslt);

    // Invoke kernel
    dim3 dimBlock(THREADS_PER_BLOCK);
    int gridX, gridY;
    if (numBlocks < 16384) {
      gridX = numBlocks;
      gridY = 1;
    } else {
      gridX = 16384;
      gridY = safe_div(numBlocks, 16384);
    }
    dim3 dimGrid(gridX, gridY);
    printf("grid.x, grid.y: %d %d\n", dimGrid.x, dimGrid.y);
    reduce_int_kernel_nonopt<<<dimGrid, dimBlock>>>
      (devInput, devOutput, curNum);
    curNum = numBlocks;
    cudaFree(devInput);
    devInput = devOutput;
  }

  rslt = cudaMemcpy(output, devOutput, sizeof(int),
                    cudaMemcpyDeviceToHost);
  if (rslt != 0) printf("failed to copy output to host: %d\n", rslt);

  cudaEventRecord(stop, 0);
  cudaEventSynchronize(stop);

  float t;
  cudaEventElapsedTime(&t, start, stop);
  printf("Time for kernel: %f\n", t);
  cudaEventDestroy(start);
  cudaEventDestroy(stop);

  cudaFree(devOutput);
}

#ifdef __cplusplus
}
#endif
