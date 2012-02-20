all:
	llc -O3 1map_sse.ll
	llc -O3 1map_nosse.ll
	as -o 1map_sse.o 1map_sse.s
	as -o 1map_nosse.o 1map_nosse.s
	gcc -O3 -msse -msse2 -msse3 -msse4.1 -msse4.2 -o 1map 1map.c 1map_sse.o 1map_nosse.o

clean:
	rm -rf *.o *.s 1map
