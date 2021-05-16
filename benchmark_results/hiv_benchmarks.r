n_cols <- c(
  153123, # 610
  397333, # 620
  545870, # 623
  787540, # 625
  1194766, # 626
  1410874 # 627
)

futhark_times = c(
  0.060, # 610
  0.141, # 620
  0.188, # 623
  0.277, # 625
  0.437, # 626
  0.514 # 627
)

openph_times = c(
  0.10675, # 610, 10
  0.33053, # 620, 10
  0.4726, # 623, 10
  0.71335, # 625, 10
  1.0325, # 626, 10
  1.33 # 627, 10
)

make_plot(n_cols, futhark_times, openph_times, "hiv")

pdf(file = "benchmarks_hiv.pdf", width = 6, height = 4)
make_plot(n_cols, futhark_times, openph_times, "hiv")
dev.off()

