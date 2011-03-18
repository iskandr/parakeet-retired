PARAKEET_PATH ?= ..
BUILDPATH = ../_build
CFLAGS = -Wall -O3
LDFLAGS = -lpthread -lm -lrt
CC = gcc
#DEBUG =
OCAMLCC = ocamlfind ocamlopt
OCAMLCFLAGS = -ccopt -fPIC
EXTLIB_DIR=$(shell ocamlfind query extlib)
UINT_DIR=$(shell ocamlfind query uint)

LIB_INCLUDES = -I $(EXTLIB_DIR) -I $(UINT_DIR)
PARAKEET_INCLUDES = -I $(BUILDPATH) \
	-I $(BUILDPATH)/Base \
	-I $(BUILDPATH)/Common \
	-I $(BUILDPATH)/Ptx \
	-I $(BUILDPATH)/GPU \
	-I $(BUILDPATH)/Q \
	-I $(BUILDPATH)/AST   \
	-I $(BUILDPATH)/Imp \
	-I $(BUILDPATH)/Imp/KernelTemplates \
	-I $(BUILDPATH)/SSA \
	-I $(BUILDPATH)/SSA/Optimizations \
 	-I $(BUILDPATH)/SSA/Analyses \
	-I $(BUILDPATH)/SSA/Specialization \
  -I $(BUILDPATH)/ShapeInference \
	-I $(BUILDPATH)/CostModel \
	-I $(BUILDPATH)/Runtime \
  -I $(BUILDPATH)/FrontEnd
OCAMLINCLUDES := $(LIB_INCLUDES) $(PARAKEET_INCLUDES)

OBJS = cqinterface.o ocaml_functions.o
COBJS = -ccopt -L/usr/lib/nvidia-current \
  $(PARAKEET_PATH)/cuda/parakeet_cuda.a \
  $(PARAKEET_PATH)/FrontEnd/parakeet.a \
	-ccopt -lcuda -ccopt -L/usr/local/cuda/lib -ccopt -lcudart \
  -ccopt $(UINT_DIR)/libuint32_stubs.a -ccopt $(UINT_DIR)/libuint64_stubs.a

OCAML_LIB_OBJECTS := bigarray.cmxa unix.cmxa str.cmxa \
                     uint32.cmxa uint64.cmx extLib.cmxa

$(shell ocamlopt ../FrontEnd/depsort.ml -o ../FrontEnd/depsort)
DEPENDENCY_SORTED_OBJECTS := $(shell ../FrontEnd/depsort -E preprocess.cmx $(PARAKEET_INCLUDES))
OCAMLOBJSRAW := $(OCAML_LIB_OBJECTS) $(DEPENDENCY_SORTED_OBJECTS)
ifneq ($(PROF),)
  OCAMLOBJS = $(patsubst %.cmx,%.p.cmx,$(OCAMLOBJSRAW))
else
  OCAMLOBJS = $(OCAMLOBJSRAW) 
endif

all: parakeetq

parakeetq: $(OBJS) 
	$(OCAMLCC) -linkall -ccopt -shared -ccopt -Wl,-Bsymbolic $(OBJS) \
	$(OCAMLINCLUDES) $(COBJS) $(DEBUG) -ccopt -Wl,--start-group $(OCAMLOBJS) \
	-ccopt -Wl,--end-group -o parakeetq.so -verbose -cclib -lstdc++

cqinterface.o: cqinterface.c cqinterface.h
	$(OCAMLCC) $(OCAMLCFLAGS) $(DEBUG) -o cqinterface.o cqinterface.c

ocaml_functions.o: ocaml_functions.c ocaml_functions.h
	gcc -fPIC $(DEBUG) -c ocaml_functions.c -I. $(OCAMLINCLUDES) \
  -o ocaml_functions.o $(LIB_INCLUDES)

clean:
	rm -f cinterface.so *~ *.o QType.cmx QType.mli QType.cmi depsort

