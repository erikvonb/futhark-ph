using DelimitedFiles


function main()
  
  if length(ARGS) < 2
    println("Too few filenames given")
  end

  lows1 = readdlm(ARGS[1], '\n', Int)
  lows2 = readdlm(ARGS[2], '\n', Int)

  # If lows* starts with -1, it's from futhark-pms, so we add 1
  # elementwise to compensate for the fact that OpenPH uses 1-based
  # indexing.
  if lows1[1] == -1
    lows1 = lows1 .+ 1
  else
    lows2 = lows2 .+ 1
  end

  if lows1 == lows2
    println("Lows match!")
  else
    println("Lows don't match")
  end

end

main()

