INCLUDES = -I/usr/lib/ocaml -I/usr/local/lib/ocaml
LLVMFLAGS = `llvm-config --cflags`
LFLAGS = -pthread
FLAGS = $(LLVMFLAGS) $(INCLUDES) $(LFLAGS)

all: llvmcpubackend

llvmcpubackend:
	g++ -fPIC -c llvm_multithreading.cpp $(FLAGS) -o llvm_multithreading.o
	gcc -fPIC -c cpu_work_queue.c $(FLAGS) -o cpu_work_queue.o
	gcc -fPIC -c ocaml_work_queue.c $(FLAGS) -o ocaml_work_queue.o

tests: llvmcpubackend
	gcc -o cpu_work_queue_test $(FLAGS) cpu_work_queue_test.c cpu_work_queue.o
