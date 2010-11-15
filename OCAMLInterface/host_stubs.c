/*
 * (c) 2009 Eric Hielscher, Alex Rubinsteyn
 *
 * OCaml wrapper for allocation/deallocation/casting of C memory.
 */

#include "caml/alloc.h"
#include "caml/bigarray.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

#include <stdint.h>
#include <stdio.h>
/*
 * These don't have anything to do with CUDA, but are allocation
 * and access functions for dealing with void* blocks from inside
 * OCaml.
 */

value ocaml_malloc(value nbytes) {
    CAMLparam1(nbytes);
    int64_t ptr = (int64_t) malloc(Int_val(nbytes));
    CAMLreturn(copy_int64(ptr));
}

value ocaml_free(value ptr) {
    CAMLparam1(ptr);
    free((void*) Int64_val(ptr));
    CAMLreturn(Val_unit);
}

// int32

value ocaml_cast_int32(value ptr) {
    CAMLparam1(ptr);
    CAMLlocal1(ocaml_i32);
    int32_t i = (int32_t) Int64_val(ptr);
    ocaml_i32 = copy_int32(i);
    CAMLreturn (ocaml_i32);
}

value ocaml_get_int32(value ptr, value idx) {
    CAMLparam2(ptr, idx);
    CAMLlocal1(ocaml_i32);
    int32_t* p = (int32_t*) Int64_val(ptr);
    int32_t i = p[Int_val(idx)];
    ocaml_i32 = copy_int32(i);
    CAMLreturn(ocaml_i32);
}

value ocaml_set_int32(value ptr, value idx, value v) {
    CAMLparam3(ptr,idx,v);
    int32_t* p = (int32_t*) Int64_val(ptr);
    p[Int_val(idx)] = copy_int32(v);
    CAMLreturn(Val_unit);
}


// OCaml's native ints

value ocaml_cast_int(value ptr) {
    CAMLparam1(ptr);
    long l = (long) Int64_val(ptr);
    CAMLreturn (Val_long(l));
}

value ocaml_get_int(value ptr, value idx) {
    CAMLparam2(ptr, idx);
    long* p = (long*) Int64_val(ptr);
    long l = p[Long_val(idx)];
    CAMLreturn(Val_long(l));
}
value ocaml_set_int(value ptr, value idx, value v) {
    CAMLparam3(ptr,idx,v);
    long* p = (long*) Int64_val(ptr);
    p[Int_val(idx)] = Long_val(v);
    CAMLreturn(Val_unit);
}

value ocaml_get_char(value ptr, value idx) {
    CAMLparam2(ptr, idx);
    char* p = (char*) Int64_val(ptr);
    char c = p[Int_val(idx)];
    CAMLreturn(Val_int(c));
}
value ocaml_set_char(value ptr, value idx, value v) {
    CAMLparam3(ptr,idx,v);
    char* p = (char*) Int64_val(ptr);
    p[Int_val(idx)] = Long_val(v);
    CAMLreturn(Val_unit);
}


// double

value ocaml_cast_double(value ptr) {
    CAMLparam1(ptr);
    double d = (double) Int64_val(ptr);
    CAMLreturn (copy_double(d));
}

value ocaml_get_double(value ptr, value idx) {
    CAMLparam2(ptr, idx);
    double* p = (double*) Int64_val(ptr);
    double d = p[Int_val(idx)];
    CAMLreturn(copy_double(d));
}

value ocaml_set_double(value ptr, value idx, value v) {
    CAMLparam3(ptr,idx,v);
    double* p = (double*) Int64_val(ptr);
    p[Int_val(idx)] = Double_val(v);
    CAMLreturn(Val_unit);
}


value ocaml_get_float(value ptr, value idx) {
    CAMLparam2(ptr, idx);
    float* p = (float*) Int64_val(ptr);
    float f = p[Int_val(idx)];
    CAMLreturn(copy_double(f));
}

value ocaml_set_float(value ptr, value idx, value v) {
    CAMLparam3(ptr,idx,v);
    float* p = (float*) Int64_val(ptr);
    p[Int_val(idx)] = Double_val(v);
    CAMLreturn(Val_unit);
}



value get_bigarray_ptr(value bigarray) {
    CAMLparam1(bigarray);
    CAMLreturn(caml_copy_int64((int64_t)Data_bigarray_val(bigarray)));
}
