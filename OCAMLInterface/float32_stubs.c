
/*
 * (c) 2009 Eric Hielscher, Alex Rubinsteyn
 *
 * OCaml wrapper for dealing with 32-bit floats
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
#include "string.h" 

// takes a float64, converts to float32, then returns its bit pattern an int32
value bits32_of_float64(value ocaml_num) {
    CAMLparam1(ocaml_num); 
    float f = (float) Double_val(ocaml_num);
    int i; 
    memcpy ((void*) &i, (void*) &f, sizeof(float));
    CAMLreturn(caml_copy_int32(i)); 
}

// takes a float64, converts to float32, then returns its bit pattern an int32
value float64_of_bits32(value ocaml_bits) {
    CAMLparam1(ocaml_bits);
    int i = Int32_val(ocaml_bits);
    float f;
    memcpy((void*) &f, (void*) &i, sizeof(float));  
    CAMLreturn(caml_copy_double((double)f)); 
}
