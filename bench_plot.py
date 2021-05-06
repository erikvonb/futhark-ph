import matplotlib.pyplot as plt
import numpy as np

# Klein diameters: 1.2 ... 1.6
n_cols = np.array(
    [32862, # 1.2
     65606, # 1.3
     117395, # 1.4
     226809, # 1.5
     360293, # 1.6
     536666, # 1.7
     739630, # 1.8
     1004681, # 1.9
     1522155, # 2.0
     ])

# futhark-pms
times = np.array(
    [215.7, # 1.2
     348.9, # 1.3
     439.7, # 1.4
     643.7, # 1.5
     789.3, # 1.6
     1274, # 1.7
     1794, # 1.8
     2129, # 1.9
     2991, #2.0, 1.2GB in nvidia-smi
     ])
fut = plt.plot(n_cols / 1000, times / 1000, 'o-', label = "Futhark")

# We run OpenPH with the following memory parameter values:
# Diam | parameter
# --------------------
# 1.2  | 10
# 1.3  | 20
# 1.4  | 20
# 1.5  | 30
# 1.6  | 30

# OpenPH without gpu reset
times = np.array(
    [272.4, # 1.2, 10
     434.0, # 1.3, 20
     716.1, # 1.4, 20
     1257.5, # 1.5, 30
     1776.9, # 1.6, 30
     2802.9, # 1.7, 30
     3895.9, # 1.8, 30
     5512.6, # 1.9, 30
     8003.3, # 2.0, 30, 2.1GB in nvidia-smi
     ])
openph1 = plt.plot(n_cols / 1000, times / 1000, 'o-', label = "OpenPH w/o GPU reset")

# OpenPH with gpu reset
times = np.array(
    [654.1, # 1.2
     896.9, # 1.3
     1177.2, # 1.4
     1713.1, # 1.5
     2158.6, # 1.6
     3167.4, # 1.7
     4267.6, # 1.8
     5890.5, # 1.9
     8336.3, # 2.0
     ])
openph2 = plt.plot(n_cols / 1000, times / 1000, 'o-', label = "OpenPH w/ GPU reset")

plt.xlabel("Number of thousand columns")
plt.ylabel("Avg runtime in s")
plt.legend()
plt.grid()
plt.show()

