n_cols <- c(
  88891, # 25
  153758, # 26
  247278, # 27
  384833, # 28
  686003, # 29
  944369, # 29.5
  1281140 # 30
)

futhark_times = c(
  0.024, # 25
  0.060, # 26
  0.067, # 27
  0.134, # 28
  0.252, # 29
  0.346, # 29.5
  0.474 # 30
)

openph_times = c(
  0.088378, # 25
  0.18543, # 26
  0.29315, # 27
  0.45405, # 28
  0.81832, # 29
  1.1638, # 29.5
  1.57 # 30
)

make_plot(n_cols, futhark_times, openph_times, "h3n2")

pdf(file = "benchmarks_h3n2.pdf", width = 6, height = 4)
make_plot(n_cols, futhark_times, openph_times, "h3n2")
dev.off()

