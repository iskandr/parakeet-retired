PARAKEET_PATH ?= ..
BUILDPATH = ../_build
CFLAGS = -Wall -O3
LDFLAGS = -lpthread -lm -lrt
CC = gcc
#DEBUG =
OCAMLCC = ocamlfind ocamlopt
OCAMLCFLAGS = -ccopt -fPIC

OBJS = debug.o

all: debug.o

debug.o: debug.c
	$(OCAMLCC) $(OCAMLCFLAGS) $(DEBUG) -o debug.o debug.c

clean:
	rm -f *~ *.o

