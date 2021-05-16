library(ggplot2)

make_plot <- function(x, y1, y2, title_str) {
  n <- length(n_cols)
  df <- data.frame(
    x = rep(n_cols / 1e3, 2),
    y = c(futhark_times, openph_times),
    alg = c(rep("Ours", n), rep("OpenPH", n))
  )
  
  ggplot(data = df, aes(x = x, y = y, group = alg)) +
    geom_line(aes(linetype = alg), size = 1) +
    geom_point(size = 3) +
    theme_bw() +
    theme(legend.position = c(0.15, 0.8),
          text = element_text(size = 18),
          plot.title = element_text(face = "bold")) +
    xlab("Number of thousand columns") +
    ylab("Runtime [s]") +
    ggtitle(title_str) +
    labs(linetype = "Algorithm") #+ xlim(150, 1500)
}
