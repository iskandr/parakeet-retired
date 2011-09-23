#!/usr/bin/python
# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt

bscuda_cpu      = [180.18, 253.52, 399.51, 689.38]
bscuda_gpu_exec = [11.94, 21.77, 45.29, 91.02]
bscuda_xfer     = [7.64, 13.34, 23.85, 46.0]

for i in range(4):
  bscuda_cpu[i] = bscuda_cpu[i] - bscuda_gpu_exec[i] - bscuda_xfer[i]

bspara_gpu_exec = [60.36, 87.7, 146.97, 266.0]
bspara_xfer     = [5.92, 6.88, 8.91, 12.29]
bspara_interp   = [106.6, 122.05, 157.39, 229.03]
bspara_ptxcomp  = [17.25, 17.07, 17.66, 17.83]

N = 4
opts = np.arange(N)
width = 0.35
space = 0.1

color1 = 'blue'
hatch1  = "\\"
color2 = 'red'
hatch2  = '//'

fig = plt.figure()
ax = fig.add_subplot(111)
cudRects1 = ax.bar(opts, bscuda_xfer, width, color=color1,
                   hatch=hatch1)
parRects1 = ax.bar(opts+width+space, bspara_xfer, width, color=color1,
                   hatch=hatch1)

cudRects2 = ax.bar(opts, bscuda_gpu_exec, width, color=color2,
                   bottom=bscuda_xfer, hatch=hatch2)
parRects2 = ax.bar(opts+width+space, bspara_gpu_exec, width, color=color2,
                   bottom=bspara_xfer, hatch=hatch2)

npbscuda_gpu_exec = np.array(bscuda_gpu_exec)
npbspara_gpu_exec = np.array(bspara_gpu_exec)
npbscuda_xfer = np.array(bscuda_xfer) + npbscuda_gpu_exec
npbspara_xfer = np.array(bspara_xfer) + npbspara_gpu_exec
cudRects3 = ax.bar(opts, bscuda_cpu, width, color='yellow',
                   bottom=npbscuda_xfer)
parRects3 = ax.bar(opts+width+space, bspara_interp, width, color='yellow',
                   bottom=npbspara_xfer)

npbspara_interp = np.array(bspara_interp) + npbspara_xfer
parRects4 = ax.bar(opts+width+space, bspara_ptxcomp, width, color='green',
                   bottom=npbspara_interp, hatch='x')


ax.set_xlabel('Number Of Data Points')
ax.set_ylabel('Time In Milliseconds')
ax.set_title('GPU K-Means Execution Time, K = 3')
ax.set_xticks(opts+(0.5*space+width))
ax.set_xticklabels(('32K', '64K', '128K', '256K'))
ax.set_yticks(np.arange(0, 900, 100))

ax.legend((cudRects1[0], cudRects2[0],
           parRects3[0], parRects4[0]),
          ('Data Transfer Time', 'GPU Execution', 'CPU Execution',
           'PTX Compilation'), loc='upper left')

def autolabel(rects, title):
  for rect in rects:
    height = rect.get_y() + rect.get_height()
    ax.text(rect.get_x()+rect.get_width()/2., 3.0+height, title,
            ha='center', va='bottom')

autolabel(cudRects3, 'CUDA')
autolabel(parRects4, 'Parakeet')

plt.show()

