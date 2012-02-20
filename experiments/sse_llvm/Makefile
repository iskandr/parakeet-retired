all:
	llc 1map_sse.ll
	llc 1map_nosse.ll
	as -o 1map_sse.o 1map_sse.s
	as -o 1map_nosse.o 1map_nosse.s
	gcc -O3 -o 1map 1map.c 1map_sse.o 1map_nosse.o

clean:
	rm -rf *.o *.s 1map
