.SUFFIXES : .cu .cu_dbg_o .c_dbg_o .cpp_dbg_o .cu_rel_o .c_rel_o .cpp_rel_o .cubin

# Add new SM Versions here as devices with new Compute Capability are released
SM_VERSIONS := sm_10 sm_11 sm_12 sm_13

CUDA_INSTALL_PATH ?= /usr/local/cuda

ifdef cuda-install
	CUDA_INSTALL_PATH := $(cuda-install)
endif

# detect OS
OSUPPER = $(shell uname -s 2>/dev/null | tr [:lower:] [:upper:])
OSLOWER = $(shell uname -s 2>/dev/null | tr [:upper:] [:lower:])
# 'linux' is output for Linux system, 'darwin' for OS X
DARWIN = $(strip $(findstring DARWIN, $(OSUPPER)))

# Basic directory setup for SDK
# (override directories only if they are not already defined)
SRCDIR     ?= 
ROOTDIR    ?= $(CUDA_SDK_ROOT_PATH)/C/src
ROOTBINDIR ?= $(ROOTDIR)/../bin
BINDIR     ?= $(ROOTBINDIR)/$(OSLOWER)
ROOTOBJDIR ?= obj
LIBDIR     := $(ROOTDIR)/../lib
COMMONDIR  := $(ROOTDIR)/../common

# Compilers
NVCC       := $(CUDA_INSTALL_PATH)/bin/nvcc 
CXX        := g++-4.3 -fPIC -ggdb3
CC         := gcc-4.3 -fPIC -ggdb3
LINK       := g++-4.3 -fPIC

# Includes
INCLUDES  += -I. -I$(CUDA_INSTALL_PATH)/include -I$(COMMONDIR)/inc
INCLUDES  += -I/usr/lib/ocaml -I../CQInterface -I$(THRUST_DIR)

# architecture flag for cubin build
CUBIN_ARCH_FLAG := -m64

# Warning flags
CXXWARN_FLAGS := \
	-W -Wall \
	-Wimplicit \
	-Wswitch \
	-Wformat \
	-Wchar-subscripts \
	-Wparentheses \
	-Wmultichar \
	-Wtrigraphs \
	-Wpointer-arith \
	-Wcast-align \
	-Wreturn-type \
	-Wno-unused-function \
	$(SPACE)

CWARN_FLAGS := $(CXXWARN_FLAGS) \
	-Wstrict-prototypes \
	-Wmissing-prototypes \
	-Wmissing-declarations \
	-Wnested-externs \
	-Wmain \

# Compiler-specific flags
NVCCFLAGS := --compiler-bindir=/usr/bin/gcc-4.3
CXXFLAGS  := $(CXXWARN_FLAGS)
CFLAGS    := $(CWARN_FLAGS)

# Common flags
COMMONFLAGS += $(INCLUDES) -DUNIX

NVCCFLAGS   += --compiler-options '-fno-strict-aliasing,-fPIC' --shared

# Debug/release configuration
ifeq ($(dbg),1)
	COMMONFLAGS += -g -D_DEBUG -DDEBUG
	NVCCFLAGS   += -D_DEBUG -DDEBUG
	BINSUBDIR   := debug
	LIBSUFFIX   := D
else
  COMMONFLAGS += -O3
	BINSUBDIR   := release
	LIBSUFFIX   :=
	NVCCFLAGS += --opencc-options -OPT:Olimit=99999
	CXXFLAGS    += -fno-strict-aliasing
  CFLAGS      += -fno-strict-aliasing
endif

 


# append optional arch/SM version flags (such as -arch sm_11)
#NVCCFLAGS += $(SMVERSIONFLAGS)

# architecture flag for cubin build
CUBIN_ARCH_FLAG := -m32

# detect if 32 bit or 64 bit system
HP_64 =	$(shell uname -m | grep 64)

# OpenGL is used or not (if it is used, then it is necessary to include GLEW)
ifeq ($(USEGLLIB),1)

	ifneq ($(DARWIN),)
		OPENGLLIB := -L/System/Library/Frameworks/OpenGL.framework/Libraries -lGL -lGLU $(COMMONDIR)/lib/$(OSLOWER)/libGLEW.a
	else
		OPENGLLIB := -lGL -lGLU

		ifeq "$(strip $(HP_64))" ""
			OPENGLLIB += -lGLEW
		else
			OPENGLLIB += -lGLEW_x86_64
		endif
	endif

	CUBIN_ARCH_FLAG := -m64
endif

ifeq ($(USEGLUT),1)
	ifneq ($(DARWIN),)
		OPENGLLIB += -framework GLUT
	else
		OPENGLLIB += -lglut
	endif
endif

ifeq ($(USEPARAMGL),1)
	PARAMGLLIB := -lparamgl$(LIBSUFFIX)
endif

ifeq ($(USERENDERCHECKGL),1)
	RENDERCHECKGLLIB := -lrendercheckgl$(LIBSUFFIX)
endif

ifeq ($(USECUDPP), 1)
	ifeq "$(strip $(HP_64))" ""
		CUDPPLIB := -lcudpp
	else
		CUDPPLIB := -lcudpp64
	endif

	CUDPPLIB := $(CUDPPLIB)$(LIBSUFFIX)

	ifeq ($(emu), 1)
		CUDPPLIB := $(CUDPPLIB)_emu
	endif
endif

# Libs
LIB       := -L$(CUDA_INSTALL_PATH)/lib -L$(LIBDIR) -L$(COMMONDIR)/lib/$(OSLOWER)
LIB += -L/usr/lib64/nvidia-current
ifeq ($(USEDRVAPI),1)
   LIB += -lcuda ${OPENGLLIB} $(PARAMGLLIB) $(RENDERCHECKGLLIB) $(CUDPPLIB) ${LIB} 
else
   LIB += -lcudart ${OPENGLLIB} $(PARAMGLLIB) $(RENDERCHECKGLLIB) $(CUDPPLIB) ${LIB}
endif

ifeq ($(USECUFFT),1)
  ifeq ($(emu),1)
    LIB += -lcufftemu
  else
    LIB += -lcufft
  endif
endif

ifeq ($(USECUBLAS),1)
  ifeq ($(emu),1)
    LIB += -lcublasemu
  else
    LIB += -lcublas
  endif
endif

# Lib/exe configuration
ifneq ($(STATIC_LIB),)
	TARGETDIR := $(LIBDIR)
	TARGET   := $(subst .a,$(LIBSUFFIX).a,$(LIBDIR)/$(STATIC_LIB))
	LINKLINE  = ar qv $(TARGET) $(OBJS) 
else
	#LIB += -lcutil$(LIBSUFFIX)
	# Device emulation configuration
	ifeq ($(emu), 1)
		NVCCFLAGS   += -deviceemu
		CUDACCFLAGS += 
		BINSUBDIR   := emu$(BINSUBDIR)
		# consistency, makes developing easier
		CXXFLAGS		+= -D__DEVICE_EMULATION__
		CFLAGS			+= -D__DEVICE_EMULATION__
	endif
	TARGETDIR := $(BINDIR)/$(BINSUBDIR)
	TARGET    := $(TARGETDIR)/$(EXECUTABLE)
	LINKLINE  = $(LINK) -o $(TARGET) $(OBJS) $(LIB)
endif

# check if verbose 
ifeq ($(verbose), 1)
	VERBOSE :=
else
	VERBOSE := @
endif

################################################################################
# Check for input flags and set compiler flags appropriately
################################################################################
ifeq ($(fastmath), 1)
	NVCCFLAGS += -use_fast_math
endif

ifeq ($(keep), 1)
	NVCCFLAGS += -keep
	NVCC_KEEP_CLEAN := *.i* *.cubin *.cu.c *.cudafe* *.fatbin.c *.ptx
endif

ifdef maxregisters
	NVCCFLAGS += -maxrregcount $(maxregisters)
endif

# Add cudacc flags
NVCCFLAGS += $(CUDACCFLAGS)

# workaround for mac os x cuda 1.1 compiler issues
ifneq ($(DARWIN),)
	NVCCFLAGS += --host-compilation=C
endif

# Add common flags
NVCCFLAGS += $(COMMONFLAGS)
CXXFLAGS  += $(COMMONFLAGS)
CFLAGS    += $(COMMONFLAGS)

ifeq ($(nvcc_warn_verbose),1)
	NVCCFLAGS += $(addprefix --compiler-options ,$(CXXWARN_FLAGS)) 
	NVCCFLAGS += --compiler-options -fno-strict-aliasing
	NVCCFLAGS += --compiler-options -ggdb3
endif

################################################################################
# Set up object files
################################################################################
OBJDIR := $(ROOTOBJDIR)/$(BINSUBDIR)
OBJS +=  $(patsubst %.cpp,$(OBJDIR)/%.cpp_o,$(notdir $(CCFILES)))
OBJS +=  $(patsubst %.c,$(OBJDIR)/%.c_o,$(notdir $(CFILES)))
OBJS +=  $(patsubst %.cu,$(OBJDIR)/%.cu_o,$(notdir $(CUFILES)))

################################################################################
# Set up cubin files
################################################################################
CUBINDIR := $(SRCDIR)data
CUBINS +=  $(patsubst %.cu,$(CUBINDIR)/%.cubin,$(notdir $(CUBINFILES)))

################################################################################
# Rules
################################################################################
$(SRCDIR)%.c_o : $(SRCDIR)%.c $(C_DEPS)
	$(VERBOSE)$(CC) $(CFLAGS) -o $@ -c $<

$(OBJDIR)/%.cpp_o : $(SRCDIR)%.cpp $(C_DEPS)
	$(VERBOSE)$(CXX) $(CXXFLAGS) -o $@ -c $<

$(SRCDIR)%.cu_o : $(SRCDIR)%.cu $(CU_DEPS)
	$(VERBOSE)$(NVCC) $(NVCCFLAGS) $(SMVERSIONFLAGS) -o $@ -c $<

$(CUBINDIR)/%.cubin : $(SRCDIR)%.cu cubindirectory
	$(VERBOSE)$(NVCC) $(CUBIN_ARCH_FLAG) $(NVCCFLAGS) $(SMVERSIONFLAGS) -o $@ -cubin $<

################################################################################
#
# Build script for project
#
################################################################################

# Add source files here
#EXECUTABLE	:= tester
# CUDA source files (compiled with cudacc)
CUFILES		:= \

# CUDA dependency files
CU_DEPS		:= \

# C/C++ source files (compiled with gcc / c++)
CCFILES		:= \

################################################################################
# Rules and targets

all: parakeet_cuda

parakeet_cuda: cuda_module_stubs.o thrust_stubs.cu_o kernel_stubs.cu_o \
    cuda_stubs.cu_o base.o
	echo $(INCLUDES)
	ar rcs parakeet_cuda.a cuda_module_stubs.o cuda_stubs.cu_o thrust_stubs.cu_o \
		kernel_stubs.cu_o base.o 

clean:
	rm -f parakeet_cuda.a *_o *.o *~

