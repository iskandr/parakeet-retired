################################################################################
#
# Copyright 1993-2006 NVIDIA Corporation.  All rights reserved.
#
# NOTICE TO USER:   
#
# This source code is subject to NVIDIA ownership rights under U.S. and 
# international Copyright laws.  
#
# NVIDIA MAKES NO REPRESENTATION ABOUT THE SUITABILITY OF THIS SOURCE 
# CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR 
# IMPLIED WARRANTY OF ANY KIND.  NVIDIA DISCLAIMS ALL WARRANTIES WITH 
# REGARD TO THIS SOURCE CODE, INCLUDING ALL IMPLIED WARRANTIES OF 
# MERCHANTABILITY, NONINFRINGEMENT, AND FITNESS FOR A PARTICULAR PURPOSE.   
# IN NO EVENT SHALL NVIDIA BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL, 
# OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS 
# OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE 
# OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE 
# OR PERFORMANCE OF THIS SOURCE CODE.  
#
# U.S. Government End Users.  This source code is a "commercial item" as 
# that term is defined at 48 C.F.R. 2.101 (OCT 1995), consisting  of 
# "commercial computer software" and "commercial computer software 
# documentation" as such terms are used in 48 C.F.R. 12.212 (SEPT 1995) 
# and is provided to the U.S. Government only as a commercial end item.  
# Consistent with 48 C.F.R.12.212 and 48 C.F.R. 227.7202-1 through 
# 227.7202-4 (JUNE 1995), all U.S. Government End Users acquire the 
# source code with only those rights set forth herein.
#
################################################################################
#
# Common build script
#
################################################################################

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
CUBIN_ARCH_FLAG := -m32

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

# Debug/release configuration
ifeq ($(dbg),1)
	COMMONFLAGS += -g
	NVCCFLAGS   += -D_DEBUG -DDEBUG
	BINSUBDIR   := debug
	LIBSUFFIX   := D
else 
	COMMONFLAGS += -O3
	BINSUBDIR   := release
	LIBSUFFIX   :=
	NVCCFLAGS   += --compiler-options '-fno-strict-aliasing,-fPIC' --shared \
	               --opencc-options -OPT:Olimit=99999
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

#all: pqlib stubs 
all: stubs 

pqlib:
	nvcc -o libpq.so --compiler-options '-fPIC' --shared \
	pqlib.cu $(INCLUDES) $(LIB) -lcuda

module_tester:
	gcc-4.3 -o module_tester module_tester.c -I. $(INCLUDES) -lcuda -lrt

tester: pqlib stubs tester.o
	gcc -g tester.o -I. $(INCLUDES) -o tester $(LIB) -L. -lpq -lpq_stubs

compaction: where.cu_o index.cu_o gen_idx.cu_o scan.cu_o compaction_tester.o \
		base.o
	gcc-4.3 where.cu_o index.cu_o gen_idx.cu_o scan.cu_o compaction_tester.o \
	base.o -o compaction $(INCLUDES) $(LIB) -L.

each: each.cu_o each_tester.o base.o
	gcc-4.3 each.cu_o each_tester.o base.o -I. $(INCLUDES) -o each $(LIB) -L.

gen_idx: gen_idx.cu_o gen_idx_tester.o base.o
	gcc-4.3 gen_idx.cu_o gen_idx_tester.o base.o -o gen_idx $(INCLUDES) $(LIB) -L.

index: index.cu_o index_tester.o base.o
	gcc-4.3 index.cu_o index_tester.o base.o -o index $(INCLUDES) $(LIB) -L.

reduce2d: reduce2d.cu_o reduce2d_tester.o base.o
	gcc-4.3 reduce2d.cu_o reduce2d_tester.o base.o -o reduce2d $(INCLUDES) $(LIB) \
	-L.

scan: scan.cu_o scan_tester.o
	gcc-4.3 scan.cu_o scan_tester.o -o scan $(INCLUDES) $(LIB) -L.

where: where.cu_o where_tester.o base.o
	gcc-4.3 where.cu_o where_tester.o base.o -o where $(INCLUDES) $(LIB) -L.

naivecpud: naivecpuapdiff.o base.o
	gcc-4.3 -O3 naivecpuapdiff.o base.o -o naivecpud $(INCLUDES) $(LIB) -L.

all_pairs: all_pairs_abs_diff.cu_o all_pairs_abs_diff_tester.o
	gcc-4.3 all_pairs_abs_diff.cu_o all_pairs_abs_diff_tester.o -I. $(INCLUDES) \
	-o all_pairs $(LIB) -L.

min_idx: min_idx_each.cu_o min_idx_each_tester.o
	gcc-4.3 min_idx_each.cu_o min_idx_each_tester.o -I. $(INCLUDES) \
	-o min_idx $(LIB) -L.

reduce: reduce.cu_o reduce_tester.o
	gcc-4.3 reduce.cu_o reduce_tester.o -I. $(INCLUDES) -o reduce $(LIB) -L.

minall: minall.cu_o minall_tester.o
	gcc-4.3 minall.cu_o minall_tester.o -I. $(INCLUDES) -o minall $(LIB) -L.

multi: multi_index_reduce.cu_o multi_index_reduce_tester.o
	gcc-4.3 multi_index_reduce.cu_o multi_index_reduce_tester.o -I. $(INCLUDES) \
		-o multi $(LIB) -L.

timing: timing.o
	gcc timing.o  libpq.so -I. $(INCLUDES) -o timing $(LIB) -L. -lpq

stubs: thrust_stubs.cu_o
	echo $(INCLUDES)
	$(CC) -c libpq_stubs.c -I. $(INCLUDES) -o libpq_stubs.o $(LIB) -L. 
	$(CC) -c cuda_stubs.c -I. $(INCLUDES) -o cuda_stubs.o $(LIB) -lcuda -lcudart
	ar rcs libpq_stubs.a libpq_stubs.o cuda_stubs.o thrust_stubs.cu_o

clean:
	rm -f libpq.so libpq_stubs.a *_o *.o tester timing *~
