/*
 * llvm_mulithreading.cpp
 *
 * C bindings for enabling LLVM multithreading support.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2012.
 */
#include <llvm/Support/Threading.h>

#include "llvm_multithreading.h"

using namespace llvm;

int LLVMStartMultithreaded() {
  return llvm_start_multithreaded();
}

void LLVMStopMultithreaded() {
  llvm_stop_multithreaded();
}
