using DelimitedFiles


function main()
  
  if length(ARGS) < 2
    println("Too few filenames given")
  end

  lows1 = readdlm(ARGS[1], '\n', Int)
  lows2 = readdlm(ARGS[2], '\n', Int)

  if lows1 == lows2
    println("Lows match!")
  else
    println("Lows don't match")
  end

end

main()

