n_cols <- c(
  154200, # .06
  401006, # .061
  679623, # .062
  917382, # .0622
  1101526, # .0626
  1447087, # .0628
  1536984 # .0629
)

futhark_times = c(
  0.064, # .06
  0.147, # .061
  0.250, # .062
  0.336, # .0622
  0.402, # .0626
  0.545, # .0628
  0.58 # .0629
)

openph_times = c(
  0.22056, # .06
  0.68172, # .061
  1.0654, # .062
  1.4018, # .0622
  1.7188, # .0626
  2.4066, # .0628
  2.68 # .0629
)

make_plot(n_cols, futhark_times, openph_times, "fractal r") + xlim(100, 1600)

pdf(file = "benchmarks_fractal.pdf", width = 6, height = 4)
make_plot(n_cols, futhark_times, openph_times, "fractal r") + xlim(100, 1600)
dev.off()

