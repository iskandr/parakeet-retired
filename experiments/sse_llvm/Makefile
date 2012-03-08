all:
	llc -O3 1map_sse.ll
	llc -O3 1map_nosse.ll
	llc -O3 map_wrapper_gen_sse.ll
	as -o 1map_sse.o 1map_sse.s
	as -o 1map_nosse.o 1map_nosse.s
	as -o map_wrapper_gen_sse.o map_wrapper_gen_sse.s
	gcc -O3 -msse -msse2 -msse3 -msse4.1 -msse4.2 -o 1map \
		1map.c 1map_sse.o 1map_nosse.o map_wrapper_gen_sse.o
	gcc -O3 -msse -msse2 -msse3 -msse4.1 -msse4.2 -o bs bs.c -lm
	gcc -O3 -msse -msse2 -msse3 -msse4.1 -msse4.2 -o bs32 bs32.c -lm

clean:
	rm -rf *.o *.s 1map
