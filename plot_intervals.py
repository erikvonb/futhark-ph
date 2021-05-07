import numpy as np
import matplotlib.pyplot as plt


intervals = np.loadtxt("intervals.txt", dtype=int)
bar_i = 0

max_death = 0
for interval in intervals:
  print(interval)
  if interval[0] == 0:
    birth = interval[1]
    death = interval[2]
    plt.plot([birth, death], [bar_i, bar_i])
    bar_i = bar_i + 1

    if death < 1e10:
      max_death = max(max_death, death)

for interval in intervals:
  print(interval)
  if interval[0] == 1:
    birth = interval[1]
    death = interval[2]
    plt.plot([birth, death], [bar_i, bar_i], '--')
    bar_i = bar_i + 1

    if death < 1e10:
      max_death = max(max_death, death)

plt.xlim(0, max_death)
plt.show()

