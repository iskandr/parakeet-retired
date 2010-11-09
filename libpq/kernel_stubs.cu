/*
 * Parallel Q Library
 *
 * (c) 2010 Eric Hielscher, Alex Rubinsteyn
 *
 * OCaml interface for Pre-defined Kernels
 */

#include <cuda.h>

#include "base.h"

#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

// TODO: This feels hackish, and breaks modularity, since we have to provide
//       stubs for the textures in the kernels we include.

extern "C" {

/** Index-related stubs **/
#include "index_kernel.cu"

CAMLprim value
ocaml_bind_index_idxs_tex(value ocaml_idxs, value ocaml_len) {
  CAMLparam2(ocaml_idxs, ocaml_len);
  int *idxs = (int*)Int32_val(ocaml_idxs);
  cudaChannelFormatDesc intDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  cudaError_t rslt = cudaBindTexture(0, indexIdxsTex, idxs, intDesc,
                                     Int_val(ocaml_len) * sizeof(int));
  check_err(rslt, "Unable to bind idxs texture");
  CAMLreturn(Val_unit);
}

CAMLprim value
ocaml_bind_index_int_vecs_tex(value ocaml_input, value ocaml_len) {
  CAMLparam2(ocaml_input, ocaml_len);
  int *input = (int*)Int32_val(ocaml_input);
  cudaChannelFormatDesc intDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  cudaError_t rslt = cudaBindTexture(0, indexIntVecsTex, input, intDesc,
                                     Int_val(ocaml_len) * sizeof(int));
  check_err(rslt, "Unable to bind idxs int vec texture");
  CAMLreturn(Val_unit);
}

CAMLprim value
ocaml_unbind_index_int_vecs_tex(void) {
  CAMLparam0();
  cudaError_t rslt = cudaUnbindTexture(indexIdxsTex);
  check_err(rslt, "Unable to unbind idxs texture");
  rslt = cudaUnbindTexture(indexIntVecsTex);
  check_err(rslt, "Unable to unbind idxs int vec texture");
  CAMLreturn(Val_unit);
}

CAMLprim value
ocaml_bind_index_float_vecs_tex(value ocaml_input, value ocaml_len) {
  CAMLparam2(ocaml_input, ocaml_len);
  float *input = (float*)Int32_val(ocaml_input);
  cudaChannelFormatDesc floatDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindFloat);
  cudaError_t rslt = cudaBindTexture(0, indexFloatVecsTex, input, floatDesc,
                                     Int_val(ocaml_len) * sizeof(float));
  check_err(rslt, "Unable to bind idxs float vec texture");
  CAMLreturn(Val_unit);
}

CAMLprim value
ocaml_unbind_index_float_vecs_tex(void) {
  CAMLparam0();
  cudaError_t rslt = cudaUnbindTexture(indexIdxsTex);
  check_err(rslt, "Unable to unbind idxs texture");
  rslt = cudaUnbindTexture(indexFloatVecsTex);
  check_err(rslt, "Unable to unbind idxs float vec texture");
  CAMLreturn(Val_unit);
}

CAMLprim value
ocaml_index_int(value ocaml_num_vecs, value ocaml_vec_len,
                value ocaml_num_idxs, value ocaml_output) {
  CAMLparam4(ocaml_num_vecs, ocaml_vec_len, ocaml_num_idxs, ocaml_output);
  int *output  = (int*)Int32_val(ocaml_output);
  int num_vecs = Int_val(ocaml_num_vecs);
  int vec_len  = Int_val(ocaml_vec_len);
  int num_idxs = Int_val(ocaml_num_idxs);
  dim3 dimBlock(THREADS_PER_LINEAR_BLOCK);
  int num_blocks = safe_div(num_vecs * vec_len, THREADS_PER_LINEAR_BLOCK);
  int gridX, gridY;
  make_linear_grid(num_blocks, &gridX, &gridY);
  dim3 dimGrid(gridX, gridY);
  index_int_kernel<<<dimGrid, dimBlock>>> (num_vecs, vec_len, num_idxs, output);
  check_err(cudaGetLastError(), "Problem launching index int");
  CAMLreturn(Val_unit);
}

CAMLprim value
ocaml_index_float(value ocaml_num_vecs, value ocaml_vec_len,
                  value ocaml_num_idxs, value ocaml_output) {
  CAMLparam4(ocaml_num_vecs, ocaml_vec_len, ocaml_num_idxs, ocaml_output);
  float *output  = (float*)Int32_val(ocaml_output);
  int num_vecs   = Int_val(ocaml_num_vecs);
  int vec_len    = Int_val(ocaml_vec_len);
  int num_idxs   = Int_val(ocaml_num_idxs);
  dim3 dimBlock(THREADS_PER_LINEAR_BLOCK);
  int num_blocks = safe_div(num_vecs * vec_len, THREADS_PER_LINEAR_BLOCK);
  int gridX, gridY;
  make_linear_grid(num_blocks, &gridX, &gridY);
  dim3 dimGrid(gridX, gridY);
  index_float_kernel<<<dimGrid, dimBlock>>>
    (num_vecs, vec_len, num_idxs, output);
  check_err(cudaGetLastError(), "Problem launching index int");
  CAMLreturn(Val_unit);
}

/** Where-related stubs **/

#include "where_kernel.cu"
CAMLprim value ocaml_bind_where_tex(value ocaml_input, value ocaml_len) {
  CAMLparam2(ocaml_input, ocaml_len);
  int *input = (int*)Int32_val(ocaml_input);
  cudaChannelFormatDesc intDesc =
    cudaCreateChannelDesc(32, 0, 0, 0, cudaChannelFormatKindSigned);
  cudaError_t rslt = cudaBindTexture(0, whereInputTex, input, intDesc,
                                     Int_val(ocaml_len) * sizeof(int));
  check_err(rslt, "Unable to bind where texture");
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_unbind_where_tex(void) {
  CAMLparam0();
  cudaError_t rslt = cudaUnbindTexture(whereInputTex);
  check_err(rslt, "Unable to unbind where texture");
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_where_tex(value ocaml_len, value ocaml_output) {
  CAMLparam2(ocaml_len, ocaml_output);
  int *output = (int*)Int32_val(ocaml_output);
  int len = Int_val(ocaml_len);
  dim3 dimBlock(THREADS_PER_LINEAR_BLOCK);
  int num_blocks = safe_div(len, THREADS_PER_LINEAR_BLOCK);
  int gridX, gridY;
  make_linear_grid(num_blocks, &gridX, &gridY);
  dim3 dimGrid(gridX, gridY);
  where_kernel<<<dimGrid, dimBlock>>> (len, output);
  check_err(cudaGetLastError(), "Problem launching where");
  CAMLreturn(Val_unit);
}

}
