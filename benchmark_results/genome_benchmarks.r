n_cols <- c(
  37580, # 27.2
  69124, # 27.4
  111331, # 27.6
  148762, # 27.7
  278675, # 27.9
  384389 # 28
)

futhark_times = c(
  0.019, # 27.2
  0.025, # 27.4
  0.043, # 27.6
  0.054, # 27.7
  0.108, # 27.9
  0.135 # 28
)

openph_times = c(
  0.06769, # 27.2
  0.10852, # 27.4
  0.14167, # 27.6
  0.2041, # 27.7
  0.37352, # 27.9
  0.51733 # 28
)

make_plot(n_cols, futhark_times, openph_times, "genome") + xlim(20, 400)

pdf(file = "benchmarks_genome.pdf", width = 6, height = 4)
make_plot(n_cols, futhark_times, openph_times, "genome") + xlim(20, 400)
dev.off()

