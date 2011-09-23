#!/usr/bin/python
# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt

bscpu1t = [179.69, 351.97, 693.39, 1379.91] # True
bscpu8t = [35.5, 71.1, 140.7, 287.7] # True
bscuda = [8.28, 15.27, 28.94, 56.55] # True

bspara = [20.0, 27.45, 41.31, 70.10] # Not true (yet)

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
cpu1tRects1 = ax.bar(opts, bscpu1t, width, color=color1)
cpu8tRects1 = ax.bar(opts+width+space, bscpu8t, width, color=color2)
cudRects1   = ax.bar(opts+2*(width+space), bscuda, width, color=color3)
parRects1   = ax.bar(opts+3*(width+space), bspara, width, color=color4)

ax.set_xlabel('Number Of Options')
ax.set_ylabel('Time In Milliseconds')
ax.set_title('Black-Scholes Execution Time')
ax.set_xticks(opts+(2*width+1.5*space))
ax.set_xticklabels(('1M', '2M', '4M', '8M'))

ax.legend((cpu1tRects1[0], cpu8tRects1[0], cudRects1[0], parRects1[0]),
          ('CPU 1 thread', 'CPU 8 threads', 'CUDA', 'Parakeet'),
          loc='upper left')

plt.show()
