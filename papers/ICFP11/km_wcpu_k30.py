#!/usr/bin/python
# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt

cpu1t = [1693.11, 3388.78, 6779.6, 13580.85]
cpu8t = [377.88, 759.89, 1520.16, 3043.94]
cuda  = [280.97, 439.98, 827.54, 1566.89]
para  = [1321.34, 1560.5, 2143.44, 3236.13]

N = 4
opts = np.arange(N)
width = 0.175
space = 0.025

color1 = 'red'
color2 = 'blue'
color3 = 'green'
color4 = 'yellow'

fig = plt.figure()
ax = fig.add_subplot(111)
cpu1tRects1 = ax.bar(opts, cpu1t, width, color=color1)
cpu8tRects1 = ax.bar(opts+width+space, cpu8t, width, color=color2)
cudRects1 = ax.bar(opts+2*(width+space), cuda, width, color=color3)
parRects1 = ax.bar(opts+3*(width+space), para, width, color=color4)

ax.set_xlabel('Number Of Data Points')
ax.set_ylabel('Time In Milliseconds')
ax.set_title('K-Means Execution Time with K = 30')
ax.set_xticks(opts+(2*width+1.5*space))
ax.set_xticklabels(('32K', '64K', '128K', '256K'))

ax.legend((cpu1tRects1[0], cpu8tRects1[0], cudRects1[0], parRects1[0]),
          ('CPU 1 thread', 'CPU 8 threads', 'CUDA', 'Parakeet'),
          loc='upper left')

plt.show()
