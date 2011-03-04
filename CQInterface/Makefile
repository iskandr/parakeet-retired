PQPATH ?= ../libpq
BUILDPATH = ../_build
CFLAGS = -Wall -O3
LDFLAGS = -lpthread -lm -lrt
CC = gcc
DEBUG =
OCAMLCC = ocamlfind ocamlopt
OCAMLCFLAGS = -ccopt -fPIC
EXTLIB_DIR=$(shell ocamlfind query extlib)
UINT_DIR=$(shell ocamlfind query uint)

LIB_INCLUDES = -I $(EXTLIB_DIR) -I $(UINT_DIR)
PARAKEET_INCLUDES = -I $(BUILDPATH) \
	-I $(BUILDPATH)/CQInterface \
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
	-I $(BUILDPATH)/Runtime
OCAMLINCLUDES := $(LIB_INCLUDES) $(PARAKEET_INCLUDES)

#COBJS = -ccopt $(PQ_PATH)/libpq.so 
OBJS = cinterface.o
COBJS = -ccopt -L/usr/lib/nvidia-current -ccopt $(PQ_PATH)/cuda_stubs.cu_o \
  $(PQ_PATH)/libpq_stubs.o $(PQ_PATH)/../OCAMLInterface/ocaml_interface.a \
	-ccopt -lcuda -ccopt -L/usr/local/cuda/lib64 -ccopt -lcudart \
	-ccopt $(PQ_PATH)/thrust_stubs.cu_o -ccopt $(PQ_PATH)/kernel_stubs.cu_o \
	-ccopt $(PQ_PATH)/base.o \
  -ccopt $(UINT_DIR)/libuint32_stubs.a -ccopt $(UINT_DIR)/libuint64_stubs.a


OCAML_LIB_OBJECTS :=  bigarray.cmxa unix.cmxa str.cmxa uint32.cmxa uint64.cmx extLib.cmxa
DEPENDENCY_SORTED_OBJECTS := $(shell ocamlopt depsort.ml -o depsort; ./depsort ls $(PARAKEET_INCLUDES))

OCAMLOBJSRAW := $(OCAML_LIB_OBJECTS) $(DEPENDENCY_SORTED_OBJECTS)

ifneq ($(PROF),)
	OCAMLOBJS = $(patsubst %.cmx,%.p.cmx,$(OCAMLOBJSRAW))
else
	OCAMLOBJS = $(OCAMLOBJSRAW) 
endif

all: cinterface 

cinterface: depsort $(OBJS) 
	$(OCAMLCC) -linkall -ccopt -shared -ccopt -Wl,-Bsymbolic $(OBJS) \
	$(OCAMLINCLUDES) $(COBJS) $(DEBUG) -ccopt -Wl,--start-group $(OCAMLOBJS) \
	-ccopt -Wl,--end-group -o cinterface.so -verbose -cclib -lstdc++

cinterface.o: cinterface.c cinterface.h
	$(OCAMLCC) $(OCAMLCFLAGS) $(DEBUG) -o cinterface.o cinterface.c

ocaml_interface.o: ocaml_interface.c ocaml_interface.h
	$(OCAMLCC) $(OCAMLCFLAGS) $(DEBUG) -o ocaml_interface.o ocaml_interface.c


clean:
	rm -f cinterface.so *~ *.o QType.cmx QType.mli QType.cmi depsort
