LLVMFLAGS = `llvm-config --cflags`

all: llvmcpubackend

llvmcpubackend:
	gcc -fPIC -c cpu_work_queue.c $(LLVMFLAGS) -o cpu_work_queue.o

