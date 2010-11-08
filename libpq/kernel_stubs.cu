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
/** Where-related stubs **/

extern "C" {

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
