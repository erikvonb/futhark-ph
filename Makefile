all: reduction.h main.c
	gcc main.c reduction.c -o main -O2 -lOpenCL -lm -pthread

reduction.h: reduction.fut sparse.fut
	futhark opencl --library reduction.fut

standalone: reduction.fut sparse.fut
	futhark opencl reduction.fut

clean:
	-rm reduction.c reduction.h

