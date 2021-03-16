using Nemo
using DelimitedFiles
using Printf


function readsparse(filename)
  coords = readdlm(filename, ' ', Int, '\n')
  n = maximum(coords[:,2]) + 1
  matrix = zeros(Int, n, n)

  for k in 1 : size(coords, 1)
    i = coords[k, 1] + 1
    j = coords[k, 2] + 1
    matrix[i, j] = 1
  end
  return matrix
end

function columnlow(c)
  for i in size(c, 1) : -1 : 1
    if (c[i] != 0)
      return i
    end
  end
  return nothing
end

function getlows(d)
  lows = zeros(Int, size(d, 2))
  for j in 1:size(d, 2)
    lowj = columnlow(d[:,j])
    lows[j] = lowj == nothing ? 0 : lowj
  end
  return lows
end

function stdalg(d)
  for j in 2 : size(d, 2)
    isreduced = false

    while !isreduced
      # Assume d_j is reduced
      isreduced = true
      
      for k in 1:(j - 1)
        lowj = columnlow(d[:,j])
        lowk = columnlow(d[:,k])
        if lowj != nothing && lowj == lowk
          d[:,j] += d[:,k]
          # If d_j wasn't reduced, set false
          isreduced = false
          break
        end
      end
    end
  end  
  return d
end

function isinjective(v)
  for i = 1 : (length(v) - 1)
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

    run(`./main -i $filename_in -o $filename_out -m`)

    println("reading original")
    # d_original´ = readdlm(filename_in,  ' ', Int, '\n')
    d_original´ = readsparse(filename_in)
    println("reading reduced")
    d_reduced´ = readdlm(filename_out, ' ', Int, '\n')
    n = size(d_original´, 1)

    Z_2, _ = FiniteField(2, 2, "x")
    S = MatrixSpace(Z_2, n, n)

    d_original = S(d_original´)
    d_reduced = S(d_reduced´)
    d_true_reduced = stdalg(d_original)

    # Transpose to find column-reduced echelon form instead of row-reduced
    _, cref_original = rref(d_original')
    _, cref_reduced  = rref(d_reduced')

    if !isinjective(getlows(d_reduced))
      println("FAILED TEST: low(⋅) not injective")
      return

    elseif getlows(d_reduced) != getlows(d_true_reduced)
      println("FAILED TEST: low(⋅) does not match std alg. Std alg gives the matrix:")
      println(d_true_reduced)
      return

    elseif cref_original != cref_reduced
      println("FAILED TEST: crefs not identical")
      return
      
    else
      println("PASSED TEST")
      passedcount += 1
    end
  end

  @printf("\nPassed %d/%d tests.\n", passedcount, totalcount)
end

main()

