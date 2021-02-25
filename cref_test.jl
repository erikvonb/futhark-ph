using Nemo
using DelimitedFiles
using Printf


function getlows(m)
  lows = zeros(Int, size(m,2))
  for j = 1:size(m,2)
    l = findlast(!iszero, m[:,j])
    if (isnothing(l))
      continue
    end
    lows[j] = l
  end
  return lows
end

function isinjective(v)
  for i = 1:(length(v) - 1)
    if v[i] > 0 && v[i] in v[i+1:end]
      return false
    end
  end
  return true
end

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

    if isinjective(getlows(d_original)) && cref_original == cref_reduced
      println("PASSED TEST")
      passedcount += 1
    else
      println("FAILED TEST")
      return
    end
  end

  @printf("\nPassed %d/%d tests.\n", passedcount, totalcount)
end

main()

