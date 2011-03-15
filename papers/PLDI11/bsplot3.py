#!/usr/bin/python
# -*- coding: utf-8 -*-

import matplotlib.pyplot as plt

bscpux = [50.627, 91.92, 170.0, 325.0]
bscudtotx = [24.0, 39.7, 71.0, 136.0]
bscudgpux = [0.387, 0.713, 1.45, 2.76]
bsustotcompx = [37.5, 54.2, 86.3, 149.9]
bsustotnocompx = [27.1, 42.3, 74.0, 139.5]
bsusgpux = [0.481, 0.94, 2.525, 4.3]

bsgputimes = [0.0043, 0.00276]
bsmemtimes = [0.126, 0.127]
bsother = [0.009, 0.006]
bscomptimes = [0.01, 0.0]

ind = range(2)
width = 0.3
xt = [(i+width)/2.0 for i in ind]

p1 = plt.bar(ind, bsgputimes, width, color='r')
#p2 = plt.bar(ind, bsmemtimes, width, color='b')
p3 = plt.bar(ind, bsother, width, color='y')
p4 = plt.bar(ind, bscomptimes, width, color='g')

plt.xticks(xt, ('CUDA SDK','Parakeet'))
plt.ylabel('Time In Seconds')
plt.title('Black-Scholes Breakdown Of Times')
#plt.legend((p1[0],p2[0],p3[0],p4[0]),
#           ('GPU Time','Mem Time','Other','Comp Time'), loc=0)
plt.legend((p1[0],p3[0],p4[0]),
           ('GPU Time','Other','Comp Time'), loc=0)

plt.show()
