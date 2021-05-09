#!/usr/bin/env python3
import numpy as np
import matplotlib.pyplot as plt
import sys

def main():
  if len(sys.argv) < 3:
    print("Give filename and dimension as arguments.")
    return

  filename = sys.argv[1]
  dimension = int(sys.argv[2])

  intervals = np.loadtxt(filename, dtype=int)
  bar_i = 0

  max_death = 0
  for interval in intervals:
    if interval[0] == dimension:
      birth = interval[1]
      death = interval[2]

      plt.plot([birth, death], [bar_i, bar_i])

      bar_i = bar_i + 1

      if death < 1e10:
        max_death = max(max_death, death)

  bar_i = 0

  for interval in intervals:
    if interval[0] == dimension:
      death = interval[2]
      if death == max_death:
        plt.arrow(death, bar_i, 1, 0, head_width=10, head_length=10,
            length_includes_head=True)
      bar_i = bar_i + 1

  plt.xlim(0, max_death)
  plt.show()

main()


