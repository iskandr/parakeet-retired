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

OCAMLINCLUDES = -I $(BUILDPATH) \
	-I $(BUILDPATH)/Base \
	-I $(BUILDPATH)/CQInterface \
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
        -I $(EXTLIB_DIR) \
        -I $(UINT_DIR)

#COBJS = -ccopt $(PQ_PATH)/libpq.so 
OBJS = cinterface.o
COBJS = -ccopt -L/usr/lib/nvidia-current -ccopt $(PQ_PATH)/cuda_stubs.cu_o \
  $(PQ_PATH)/libpq_stubs.o $(PQ_PATH)/../OCAMLInterface/ocaml_interface.a \
	-ccopt -lcuda -ccopt -L/usr/local/cuda/lib64 -ccopt -lcudart \
	-ccopt $(PQ_PATH)/thrust_stubs.cu_o -ccopt $(PQ_PATH)/kernel_stubs.cu_o \
	-ccopt $(PQ_PATH)/base.o \
  -ccopt $(UINT_DIR)/libuint32_stubs.a -ccopt $(UINT_DIR)/libuint64_stubs.a


OCAML_LIB_OBJECTS :=  bigarray.cmxa unix.cmxa uint32.cmxa uint64.cmx extLib.cmxa
#OCAML_BUILT_OBJECTS :=   $(shell find $(PQ_PATH)/../_build/ -name '*.cmx' | xargs -n1 basename ) 
#OCAMLDEP := ocamldep -pp 'camlp4o pa_macro.cmo' 
#DEPENDENCY_SORTED_OBJECTS := $(shell ls -R $(PQ_PATH)/../* | grep '\.ml$$' | xargs ocamldsort -opt -d $(OCAMLDEP))
#OCAML_FILTERED_OBJECTS := $(filter $(OCAML_BUILT_OBJECTS), $(DEPENDENCY_SORTED_OBJECTS))
#OCAMLOBJSRAW := $(OCAML_LIB_OBJECTS) $(OCAML_FILTERED_OBJECTS)

OCAMLOBJSRAW := bigarray.cmxa unix.cmxa uint32.cmxa uint64.cmx extLib.cmxa \
BaseCommon.cmx BaseArray.cmx BaseList.cmx  BaseSet.cmx  BaseMap.cmx \
Graph.cmx \
Bool.cmx Float.cmx Int.cmx PSet.cmx BasePMap.cmx Base.cmx \
MutableSet.cmx \
Timing.cmx \
DynType.cmx \
UID.cmx \
ID.cmx \
PQNum.cmx \
Prim.cmx \
QPrims.cmx \
SourceInfo.cmx QSyntax.cmx AST_Info.cmx AST.cmx QSyntax_to_AST.cmx \
QParser.cmx \
QLexer.cmx  \
FnId.cmx StmtId.cmx \
Block.cmx \
SSA.cmx \
SSA_Codegen.cmx \
AST_to_SSA.cmx \
SSA_Analysis.cmx \
SSA_Transform.cmx \
FnTable.cmx \
ConstantLattice.cmx \
FindBindingSet.cmx \
FindUseCounts.cmx \
FindDefs.cmx \
FindLiveIds.cmx \
TypeCheck.cmx \
Replace.cmx \
FindConstants.cmx \
Inline.cmx \
Simplify.cmx \
AdverbFusion.cmx \
CSE.cmx \
RunOptimizations.cmx \
Signature.cmx \
InterpState.cmx \
TypeInfer.cmx \
CollectPartialApps.cmx \
TypeAnalysis.cmx \
RewriteTyped.cmx \
Specialize.cmx \
PtxType.cmx PtxVal.cmx Ptx.cmx \
Shape.cmx \
Cuda.cmx \
HostVal.cmx GpuVal.cmx InterpVal.cmx \
MemoryState.cmx \
LibPQ.cmx \
PtxSimplify.cmx PtxTidy.cmx PtxCodegen.cmx \
Imp.cmx \
ImpSimplify.cmx \
SymbolicShape.cmx \
ImpReplace.cmx \
ImpCodegen.cmx \
ImpAllPairsTemplate.cmx \
ImpMapTemplate.cmx \
ImpReduceTemplate.cmx \
ImpToPtx.cmx \
ShapeInference.cmx \
ShapeEval.cmx \
SSA_to_Imp.cmx \
QType.cmx HardwareInfo.cmx \
PtxCallingConventions.cmx \
GpuRuntime.cmx \
QStdLib.cmx \
GpuCost.cmx \
CostModel.cmx \
Eval.cmx \
CodeTemplate.cmx Callbacks.cmx

ifneq ($(PROF),)
	OCAMLOBJS = $(patsubst %.cmx,%.p.cmx,$(OCAMLOBJSRAW))
else
	OCAMLOBJS = $(OCAMLOBJSRAW) 
endif

all: cinterface 

cinterface: $(OBJS)
	@echo "!!! lib objects: $(OCAML_LIB_OBJECTS)" 
	@echo "!!! built: $(OCAML_BUILT_OBJECTS)"
	@echo "!!! dependency: $(DEPENDENCY_SORTED_OBJECTS)"
	@echo "!!! filtered: $(OCAML_FILTERED_OBJECTS)"
	@echo "shell ocamldsort $(OCAMLINCLUDES) -d $(OCAMLDEP) $(PQ_PATH)/../AST/*.ml"
	$(OCAMLCC) -linkall -ccopt -shared -ccopt -Wl,-Bsymbolic $(OBJS) \
	$(OCAMLINCLUDES) $(COBJS) $(DEBUG) -ccopt -Wl,--start-group $(OCAMLOBJS) \
	-ccopt -Wl,--end-group -o cinterface.so -verbose -cclib -lstdc++

cinterface.o: cinterface.c cinterface.h
	$(OCAMLCC) $(OCAMLCFLAGS) $(DEBUG) -o cinterface.o cinterface.c

ocaml_interface.o: ocaml_interface.c ocaml_interface.h
	$(OCAMLCC) $(OCAMLCFLAGS) $(DEBUG) -o ocaml_interface.o ocaml_interface.c

clean:
	rm -f cinterface.so *~ *.o QType.cmx QType.mli QType.cmi
