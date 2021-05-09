#!/usr/bin/env julia
using DelimitedFiles
using Printf

function prepare_file(filename_in, filename_out)
  matrix = readdlm(filename_in, ' ', Int, '\n')

  open(filename_out, "w") do fp
    max_j = 0

    # Column indices
    write(fp, "[")
    for i = 1:(size(matrix, 1) - 1)
      c = matrix[i, 2]
      max_j = max(max_j, c)
      write(fp, @sprintf "%d,\n" c)
    end
    write(fp, @sprintf "%d]\n\n" matrix[end, 2])

    # Row indices
    write(fp, "[")
    for i = 1:(size(matrix, 1) - 1)
      r = matrix[i, 1]
      max_j = max(max_j, r)
      write(fp, @sprintf "%d,\n" r)
    end
    write(fp, @sprintf "%d]\n\n" matrix[end, 1])

    # Matrix size
    write(fp, @sprintf "%di64\n" max_j)
  end
end

function make_futhark_file(filenames)
  open("benchmark.fut",  "w") do fp
    write(fp, """import "reduction"\n""")
    write(fp, "-- ==\n")
    for filename in filenames
      write(fp, "-- input @ $filename\n")
    end
    write(fp, """
          let main (col_idxs: []i32) (row_idxs: []i32) (n: i64): []i64 =
            let s = init_state col_idxs row_idxs n |> reduce_state
            in s.lows
          """)
  end
end

function main()

  if length(ARGS) == 0
    println("No filename given.")
    return
  end

  filenames_in = ARGS
  filenames_out = map(x -> "bench_" * splitpath(x)[end], filenames_in)

  map(prepare_file, filenames_in, filenames_out)
  make_futhark_file(filenames_out)

  run(`futhark bench benchmark.fut --backend=opencl --json=bench0.json --runs=10`)
  cmd = pipeline(`cat bench0.json`, `json_pp`)
  run(pipeline(cmd, stdout="bench.json"))
  run(`sed -i 's/\\n/\n/g' bench.json`)
  
  map(rm, filenames_out)
  rm("benchmark.fut")
  rm("benchmark.c")
  rm("benchmark")
  rm("bench0.json")

end

main()

