BUILDPATH = ../_build
CFLAGS = -Wall -m32 -O3
LDFLAGS = -lpthread -lm -lrt
CC = gcc
DEBUG =
OCAMLCC = ocamlopt
OCAMLCFLAGS = -ccopt -fPIC

OCAMLINCLUDES = -I $(BUILDPATH) -I $(BUILDPATH)/Analysis \
	-I $(BUILDPATH)/CQInterface -I $(BUILDPATH)/Dynamic -I $(BUILDPATH)/Base \
	-I $(BUILDPATH)/Ptx -I $(BUILDPATH)/GPU -I $(BUILDPATH)/Q -I $(BUILDPATH)/ResourceManager \
	-I $(BUILDPATH)/Dump \
	-I $(BUILDPATH)/extlib -I $(BUILDPATH)/AST   \
	-I $(BUILDPATH)/Imp \
	-I $(BUILDPATH)/UntypedSSA \
	-I $(BUILDPATH)/SSA
   
#COBJS = -ccopt $(PQ_PATH)/libpq.so 
OBJS = cinterface.o
COBJS = -ccopt -L/usr/lib/nvidia-current -ccopt $(PQ_PATH)/cuda_stubs.o $(PQ_PATH)/libpq_stubs.o $(PQ_PATH)/../OCAMLInterface/ocaml_interface.a -ccopt -lcuda

#OCAMLOBJS := $(shell find ../_build/ -name '*.cmx' | xargs -n1 basename ) 

OCAMLOBJSRAW := unix.cmxa enum.cmx dynArray.cmx extString.cmx extHashtbl.cmx \
extList.cmx PMap.cmx std.cmx bitSet.cmx extArray.cmx option.cmx \
BaseCommon.cmx Tuple.cmx BaseArray.cmx BaseList.cmx  BaseSet.cmx  BaseMap.cmx \
Graph.cmx \
Bool.cmx Float.cmx Int.cmx Float32.cmx PSet.cmx BasePMap.cmx Base.cmx \
Dict.cmx \
UID.cmx ID.cmx PQNum.cmx Prim.cmx DynType.cmx \
QPrims.cmx \
SourceInfo.cmx QSyntax.cmx AST_Info.cmx AST.cmx QSyntax_to_AST.cmx \
QParser.cmx \
QLexer.cmx  \
SSA_Gates.cmx SSA.cmx AST_to_SSA.cmx \
ConstantLattice.cmx \
UntypedAnalysis.cmx \
UntypedFindConstants.cmx UntypedFindDefs.cmx \
UntypedFindGenSet.cmx UntypedFindUseCounts.cmx \
UntypedReplace.cmx UntypedInline.cmx \
UntypedFindUseSets.cmx UntypedElimDeadCode.cmx \
UntypedSimplify.cmx UntypedElimPartialApps.cmx \
UntypedSimpleCSE.cmx \
TypedFindLiveIds.cmx \
TypedElimDeadCode.cmx \
AdverbFusion.cmx \
RunOptimizations.cmx \
TypeInfer.cmx \
Signature.cmx \
Program.cmx \
SSA_Codegen.cmx \
InsertCoercions.cmx \
Specialize.cmx \
HostPtr.cmx bigarray.cmxa PtxType.cmx PtxVal.cmx Ptx.cmx Shape.cmx \
HostVal.cmx GpuVal.cmx DynVal.cmx \
MemoryState.cmx \
PtxHelpers.cmx LibPQ.cmx \
PtxSimplify.cmx PtxTidy.cmx PtxCodegen.cmx  \
Imp.cmx ImpCommon.cmx ImpCodegen.cmx ImpGenAllPairs.cmx \
ImpGenMap.cmx ImpGenReduce.cmx ImpAbstractInterp.cmx ImpInferMemspace.cmx ImpToPtx.cmx \
SSA_to_Imp.cmx \
QType.cmx HardwareInfo.cmx Init.cmx \
GpuRuntime.cmx \
Eval_SSA.cmx \
CodeTemplate.cmx Callbacks.cmx 


#$(shell ls $(BUILDPATH) -R | grep cmx | tr '\n' ' ')

ifneq ($(PROF),)
	OCAMLOBJS = $(patsubst %.cmx,%.p.cmx,$(OCAMLOBJSRAW))
	#OCAMLOBJS = $(patsubst %.cmxa,%.p.cmxa,$(OCAMLOBJSAUX1))
else
	OCAMLOBJS = $(OCAMLOBJSRAW) 
endif

all: cinterface 

cinterface: $(OBJS)
	$(OCAMLCC) -ccopt -shared -ccopt -Wl,-Bsymbolic $(OBJS) \
	$(OCAMLINCLUDES) $(COBJS) $(DEBUG) -ccopt -Wl,--start-group $(OCAMLOBJS) \
	-ccopt -Wl,--end-group -o cinterface.so -verbose

cinterface.o: cinterface.c cinterface.h
	$(OCAMLCC) $(OCAMLCFLAGS) $(DEBUG) -o cinterface.o cinterface.c

ocaml_interface.o: ocaml_interface.c ocaml_interface.h
	$(OCAMLCC) $(OCAMLCFLAGS) $(DEBUG) -o ocaml_interface.o ocaml_interface.c

clean:
	rm -f cinterface.so *~ *.o QType.cmx QType.mli QType.cmi
