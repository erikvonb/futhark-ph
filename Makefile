all: reduction.h main.c
	gcc main.c reduction.c -o main -O2 -lOpenCL -lm -pthread

reduction.h: reduction.fut
	#futhark cuda --library reduction.fut
	futhark opencl --library reduction.fut

clean:
	-rm reduction.c reduction.h

