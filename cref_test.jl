using Nemo
using DelimitedFiles
using Printf


function main()

  if length(ARGS) == 0
    println("No filename given.")
    return
  end

  totalcount = 0
  passedcount = 0
  
  for filename in ARGS

    if occursin("_reduced", filename)
      continue
    end

    totalcount += 1

    filename_in = filename
    filename_out = split(filename_in, '.')[1] * "_reduced.txt"

    print("\nChecking file ")
    println(filename_in)
    println(filename_out)

    run(`./main $filename_in $filename_out`)

    d_original = readdlm(filename_in,  ' ', Int, '\n')
    d_reduced  = readdlm(filename_out, ' ', Int, '\n')
    n = size(d_original, 1)

    Z_2, _ = FiniteField(2, 2, "x")
    S = MatrixSpace(Z_2, n, n)

    # Transpose to find column-reduced echelon form instead of row-reduced
    _, cref_original = rref(S(d_original'))
    _, cref_reduced  = rref(S(d_reduced'))

    # println("CREF of original matrix is:")
    # display(cref_original')
    # println("")

    # println("CREF of reduced matrix is:")
    # display(cref_reduced')
    # println("")

    if cref_original == cref_reduced
      println("PASSED TEST")
      passedcount += 1
    else
      println("FAILED TEST")
    end
  end

  @printf("\nPassed %d/%d tests.\n", passedcount, totalcount)
end

main()

