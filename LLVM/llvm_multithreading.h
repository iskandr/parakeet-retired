/*
 * llvm_mulithreading.h
 *
 * C bindings for enabling LLVM multithreading support.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2012.
 */

#ifndef _LLVM_MULTITHREADING_H_
#define _LLVM_MULTITHREADING_H_

#ifdef __cplusplus
extern "C" {
#endif

int LLVMStartMultithreaded();
void LLVMStopMultithreaded();

#ifdef __cplusplus
}
#endif

#endif
