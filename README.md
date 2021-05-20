# Persistent Homology in Futhark

## Running
When compiled with `make`, the `main` executable can read a sparse boundary matrix in COO format, and writes either the resulting array of lows, the persistence intervals, or the entire reduced matrix, depending on the flags given.
The input file should contain rows of the format `i j` where `i` and `j` are, respectively, the row and column indices of a nonzero element in the (binary) boundary matrix.

Alternatively, `make standalone` compiles the Futhark code directly into an executable.
The resulting executable, `reduction`, can read a boundary matrix as a Futhark-style input file.
The input file should contain, in order,
1. an array of the column indices of each nonzero element,
2. an array of the row indices of each nonzero element,
3. the number of columns in the matrix.
The arrays should be in Futhark syntax, e.g. `[0,1,2,3]`, and the number of columns should be of type `i64`, e.g. `12i64`.

## Testing correctness
The Julia script `cref_test.jl` reads a list of COO matrices from a file, reduces it, and checks that
1. the matrix is actually reduced, i.e. that `low()` is injective on the nonzero columns,
2. `low()` on the reduced matrix from our algorithm matches the `low()` on the reduction from the standard algorithm, and
3. that the CREF of the reduced matrix equals the CREF of the original matrix, to convince us that only column additions have taken place and no other unexpected transformations.
Run `./cref_test.jl test-matrices-sparse/*` to check correctness on 50 specific randomly generated Vietoris-Rips boundary matrices.

## Benchmarking
The Julia script `benchmark.jl` reads list of COO matrices from a file, converts them into Futhark syntax, and runs `futhark bench` on them.
The benchmark times are printed to stdout and saved to `bench.json`.
Run `./benchmark.jl benchmark-matrices/*` to benchmark on 7 matrices with about 10^6 columns each.

