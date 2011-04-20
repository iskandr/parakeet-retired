/*
 * parakeet.c
 *
 * Main routines for interfacing front ends with the Parakeet runtime.
 *
 * (c) Eric Hielscher and Alex Rubinsteyn, 2009-2011.
 */
#include "parakeet.h"

static int parakeet_inited = 0;

void parakeet_init(void) {
  if (parakeet_inited) return;

  parakeet_inited = 1;
  
  char *argv[] = {"argv", 0};
  caml_startup(argv);

  ast_init();
  front_end_init();
}
