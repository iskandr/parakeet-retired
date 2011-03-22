#!/usr/bin/python
# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt

# TODO: Dummy data basically for now while I fuck with the plots
#bscpux = [50.627, 91.92, 170.0, 325.0]
#bscudtotx = [24.0, 39.7, 71.0, 136.0]
#bscudgpux = [0.387, 0.713, 1.45, 2.76]
##bsuscomp = [37.5, 54.2, 86.3, 149.9]
#bsustotnocompx = [27.1, 42.3, 74.0, 139.5]
#bsusgpux = [0.481, 0.94, 2.525, 4.3]

bscpu  = [50.0, 91.0, 170.0, 325.0]
bscuda_exec = [1.0, 3.0, 5.0, 8.0]
bscuda_xfer = [11.0, 33.0, 50.0, 70.0]
bspara_exec = [1.2, 3.3, 6.0, 8.5]
bspara_xfer = [11.0, 33.0, 50.0, 70.0]

N = 4
opts = np.arange(N)
width = 0.35
space = 0.1

color1 = 'red'
hatch1  = "\\"
color2 = 'blue'
hatch2  = '//'

fig = plt.figure()
ax = fig.add_subplot(111)
cudRects1 = ax.bar(opts, bscuda_exec, width, color=color1,
                   hatch=hatch1)
parRects1 = ax.bar(opts+width+space, bspara_exec, width, color=color1,
                   hatch=hatch1)

cudRects2 = ax.bar(opts, bscuda_xfer, width, color=color2,
                   bottom=bscuda_exec, hatch=hatch2)
parRects2 = ax.bar(opts+width+space, bspara_xfer, width, color=color2,
                   bottom=bspara_exec, hatch=hatch2)

ax.set_xlabel('Number Of Options')
ax.set_ylabel('Time In Milliseconds')
ax.set_title('Black-Scholes Execution Time')
ax.set_xticks(opts+(0.5*space+width))
ax.set_xticklabels(('1M', '2M', '4M', '8M'))
ax.set_yticks(np.arange(0, 100, 10))

ax.legend((cudRects1[0], cudRects2[0]),
          ('Execution Time', 'Data Transfer Time'), loc='upper left')

def autolabel(rects, title):
  for rect in rects:
    height = rect.get_y() + rect.get_height()
    ax.text(rect.get_x()+rect.get_width()/2., 3.0+height, title,
            ha='center', va='bottom', size='x-small')

autolabel(cudRects2, 'CUDA')
autolabel(parRects2, 'Parakeet')

plt.show()

