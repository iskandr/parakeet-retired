OBJ_SUFFIX ?= cmx
OBJ_ARCHIVE_SUFFIX ?= cmxa
OCAML_COMPILER ?= ocamlopt  
OCAMLCC = ocamlfind $(OCAML_COMPILER) 
PARAKEET_PATH ?= ..
BUILDPATH = ../_build
CFLAGS = -Wall -O3
LDFLAGS = -lpthread -lm -lrt
CC = gcc
DEBUG =
OCAMLCFLAGS = -ccopt -fPIC
EXTLIB_DIR=$(shell ocamlfind query extlib)
LLVM_DIR = /usr/local/lib/ocaml
LIB_INCLUDES = -I $(EXTLIB_DIR)
#-I $(BUILDPATH)/Imp/KernelTemplates \
#-I $(BUILDPATH)/CostModel \
#-I $(BUILDPATH)/Python \
#-I $(BUILDPATH)/Ptx \   
PARAKEET_INCLUDES = -I $(BUILDPATH) \
	-I $(LLVM_DIR) \
	-I $(BUILDPATH)/Imp \
	-I $(BUILDPATH)/ShapeInference \
	-I $(BUILDPATH)/Base \
	-I $(BUILDPATH)/Common \
	-I $(BUILDPATH)/AST   \
	-I $(BUILDPATH)/SSA \
	-I $(BUILDPATH)/SSA/Optimizations \
 	-I $(BUILDPATH)/SSA/Analyses \
	-I $(BUILDPATH)/SSA/Specialization \
  	-I $(BUILDPATH)/Runtime \
	-I $(BUILDPATH)/Runtime/Cuda \
	-I $(BUILDPATH)/Runtime/LLVM \
	-I $(BUILDPATH)/Runtime/Memory \
	-I $(BUILDPATH)/FrontEnd
  
OCAMLINCLUDES := $(LIB_INCLUDES) $(PARAKEET_INCLUDES)

COMMON = `ls $(PARAKEET_PATH)/Common/*.o`
FRONTEND = `ls $(PARAKEET_PATH)/FrontEnd/*.o`
OBJS =
COBJS = -ccopt -L/usr/lib/nvidia-current \
  $(PARAKEET_PATH)/cuda/parakeet_cuda.a $(COMMON) $(FRONTEND) \
	-ccopt -lcuda \
        -ccopt -L/usr/local/cuda/lib \
        -ccopt -L/usr/local/cuda/lib64 \
        -ccopt -lcudart 
 

OCAML_LIB_OBJECTS := bigarray.$(OBJ_ARCHIVE_SUFFIX) \
  unix.$(OBJ_ARCHIVE_SUFFIX) \
  str.$(OBJ_ARCHIVE_SUFFIX) \
  extLib.$(OBJ_ARCHIVE_SUFFIX) \
  xml-light.$(OBJ_ARCHIVE_SUFFIX)

$(shell ocamlopt unix.cmxa ../FrontEnd/depsort.ml -o ../FrontEnd/depsort)
#$(echo Determining OCaml module dependencies)
DEPENDENCY_SORTED_OBJECTS := $(shell ../FrontEnd/depsort -S $(OBJ_SUFFIX) -E Callbacks.$(OBJ_SUFFIX) $(PARAKEET_INCLUDES))

OCAMLOBJSRAW := $(OCAML_LIB_OBJECTS) $(DEPENDENCY_SORTED_OBJECTS) Callbacks.$(OBJ_SUFFIX)
ifneq ($(PROF),)
  OCAMLOBJS = $(patsubst %.$(OBJ_SUFFIX),%.$(OBJ_SUFFIX).cmx,$(OCAMLOBJSRAW))
else
  OCAMLOBJS = $(OCAMLOBJSRAW)
endif

all: parakeetpy

parakeetpy: $(OBJS)
	echo "Building Python Parakeet library"
	echo "OCAML OBJS: " $(OCAMLOBJS)
	$(OCAMLCC) -linkall -ccopt -shared -ccopt -Wl,-Bsymbolic $(OBJS) \
	$(OCAMLINCLUDES) $(COBJS) $(DEBUG) -ccopt -Wl,--start-group $(OCAMLOBJS) \
	-ccopt -Wl,--end-group -o libparakeetpy.so -verbose -cclib -lstdc++

clean:
	rm -f *.so *~ *.o

