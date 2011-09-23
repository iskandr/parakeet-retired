#ifndef _DEBUG_H_

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value ocaml_inspect_block(value v);

#endif
