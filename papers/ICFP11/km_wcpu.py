#!/usr/bin/python
# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt

cpu1t = [179.69, 351.97, 693.39, 1379.91]
cpu8t = [35.5, 71.1, 140.7, 287.7]
cuda = [19.8, 37.3, 72.5, 143.1]
para = [22.2, 54.0, 88.0, 160.0]

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

ax.set_xlabel('Number Of ')
ax.set_ylabel('Time In Milliseconds')
ax.set_title('K-Means Execution Time')
ax.set_xticks(opts+(2*width+1.5*space))
ax.set_xticklabels(('1M', '2M', '4M', '8M'))

ax.legend((cpu1tRects1[0], cpu8tRects1[0], cudRects1[0], parRects1[0]),
          ('CPU 1 thread', 'CPU 8 threads', 'CUDA', 'Parakeet'),
          loc='upper left')

plt.show()
