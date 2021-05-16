n_cols <- c(
  82226, # .015
  242621, # .018
  449401, # .02
  827494, # .022
  1111166, # .023
  1477668, # .024
  1940952, # .025
  2519871 # .026
)

futhark_times = c(
  0.067, # .015
  0.173, # .018
  0.217, # .02
  0.389, # .022
  0.663, # .023
  0.719, # .024
  1.289, # .025
  2.03 # .026
)

openph_times = c(
  0.154, # .015, 10
  1.4813, # .018, 20
  2.1553, # .02, 20
  3.7419, # .022, 30
  5.149, # .023, 30
  6.3665, # .024, 30
  8.3569, # .025, 30
  10.22 # .026, 50
)

make_plot(n_cols, futhark_times, openph_times, "dragon")

pdf(file = "benchmarks_dragon.pdf", width = 6, height = 4)
make_plot(n_cols, futhark_times, openph_times, "dragon")
dev.off()

