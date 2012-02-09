LLVMFLAGS = `llvm-config --cflags`
LFLAGS = -pthread
FLAGS = $(LLVMFLAGS) $(LFLAGS)

all: llvmcpubackend

llvmcpubackend:
	gcc -fPIC -c cpu_work_queue.c $(FLAGS) -o cpu_work_queue.o

tests: llvmcpubackend
	gcc -o cpu_work_queue_test $(FLAGS) cpu_work_queue_test.c cpu_work_queue.o
