#!/usr/bin/python
# -*- coding: utf-8 -*-

import matplotlib.pyplot as plt



x = []


plt.plot(x, bsusgpux, label='Parakeet GPU Time')
plt.plot(x, bscudgpux, label='CUDA GPU Time')

plt.xlabel('Number Of Options')
plt.ylabel('Time In Milliseconds')
plt.title('Black-Scholes GPU Time')
plt.legend(loc=2)
plt.show()
