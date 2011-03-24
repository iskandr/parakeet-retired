#!/usr/bin/python
# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt

bscuda_exec = [0.45, 0.81, 1.58, 3.16]
bscuda_xfer = [7.82, 14.44, 27.35, 53.38]
bspara_gpu_exec = [0.52, 0.96, 1.98, 3.76]
bspara_xfer = [7.69, 14.16, 26.91, 52.91]
bspara_interp = [3.95, 4.12, 4.29, 4.79]
bspara_init = [1.66, 1.80, 1.53, 1.40]
bspara_ptxcomp = [7.09, 7.17, 6.81, 6.77]

for i in range(4):
  bspara_interp[i] -= bspara_init[i]

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

cudRects2 = ax.bar(opts, bscuda_exec, width, color=color2,
                   bottom=bscuda_xfer, hatch=hatch2)
parRects2 = ax.bar(opts+width+space, bspara_gpu_exec, width, color=color2,
                   bottom=bspara_xfer, hatch=hatch2)

npbspara_gpu_exec = np.array(bspara_gpu_exec)
npbspara_xfer = np.array(bspara_xfer) + npbspara_gpu_exec
parRects3 = ax.bar(opts+width+space, bspara_interp, width, color='brown',
                   bottom=npbspara_xfer)

npbspara_interp = np.array(bspara_interp) + npbspara_xfer
parRects4 = ax.bar(opts+width+space, bspara_init, width, color='yellow',
                   bottom=npbspara_interp, hatch='|')

npbspara_init = np.array(bspara_init) + npbspara_interp
parRects5 = ax.bar(opts+width+space, bspara_ptxcomp, width, color='green',
                   bottom=npbspara_init, hatch='x')


ax.set_xlabel('Number Of Options')
ax.set_ylabel('Time In Milliseconds')
ax.set_title('GPU Black-Scholes Execution Time')
ax.set_xticks(opts+(0.5*space+width))
ax.set_xticklabels(('1M', '2M', '4M', '8M'))
ax.set_yticks(np.arange(0, 100, 10))

ax.legend((cudRects1[0], cudRects2[0],
           parRects3[0], parRects4[0], parRects5[0]),
          ('Data Transfer Time', 'Execution Time', 'Interpreter',
           'Initialization', 'PTX Compilation'), loc='upper left')

def autolabel(rects, title):
  for rect in rects:
    height = rect.get_y() + rect.get_height()
    ax.text(rect.get_x()+rect.get_width()/2., 3.0+height, title,
            ha='center', va='bottom')

autolabel(cudRects2, 'CUDA')
autolabel(parRects5, 'Parakeet')

plt.show()

