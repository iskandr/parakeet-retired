#!/usr/bin/python
# -*- coding: utf-8 -*-

import matplotlib.pyplot as plt

bscpux = [50.627, 91.92, 170.0, 325.0]
bscudtotx = [24.0, 39.7, 71.0, 136.0]
bscudgpux = [0.387, 0.713, 1.45, 2.76]
bsustotcompx = [37.5, 54.2, 86.3, 149.9]
bsustotnocompx = [27.1, 42.3, 74.0, 139.5]
bsusgpux = [0.481, 0.94, 2.525, 4.3]

x = [pow(2,20), pow(2,21), pow(2,22), pow(2,23)]


#plt.plot(x, bsusgpux, label='Parakeet GPU Time')
#plt.plot(x, bscudgpux, label='CUDA GPU Time')

plt.xlabel('Number Of Options')
plt.ylabel('Time In Milliseconds')
plt.title('Black-Scholes Total Time')
plt.plot(x, bscpux, 'ro-', label='CPU')
plt.plot(x, bscudtotx, 'g^-', label='CUDA SDK')
plt.plot(x, bsustotcompx, 'b--', label='Parakeet w/ Comp')
plt.plot(x, bsustotnocompx, label='Parakeet w/o Comp')
plt.legend(loc=2)
plt.show()
