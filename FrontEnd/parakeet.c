/*
 * parakeet.c
 *
 * Main routines for interfacing front ends with the Parakeet runtime.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2011.
 */
#include "parakeet.h"

void parakeet_init(void) {
  char *argv[] = {"argv", 0};
  caml_startup(argv);
}

