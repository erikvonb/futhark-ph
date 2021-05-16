n_cols <- c(
  160826, # .086
  364867, # .088
  478533, # .089
  706890, # .09
  1051229, # .0914
  1444459 # .0915
)

futhark_times = c(
  0.076, # .086
  0.148, # .088
  0.195, # .089
  0.278, # .09
  0.422, # .0914
  0.60 # .0915
)

openph_times = c(
  0.15778, # .086, 10
  0.31151, # .088, 10
  0.47514, # .089, 10
  0.72295, # .09, 10
  1.0994, # .0914, 10
  1.37 # .0915, 10
)

make_plot(n_cols, futhark_times, openph_times, "celegans") + xlim(150, 1600)

pdf(file = "benchmarks_celegans.pdf", width = 6, height = 4)
make_plot(n_cols, futhark_times, openph_times, "celegans") + xlim(150, 1600)
dev.off()

