#!/usr/bin/python

import matplotlib.pyplot as plt

infile = open("KMCPU4-500.txt")
infile.readline()
infile.readline()

lines = [line.split() for line in infile.readlines()]
data = [(int(line[0]),int(line[1]),int(line[2]),float(line[3]))
        for line in lines]

dataL3K3raw = [(pt[0],pt[3]) for pt in data if pt[1] == 300 and pt[2] == 30]

def avg(a,b):
  return (a[0], (a[1]+b[1])/2.0)

dataL3K3 = [avg(dataL3K3raw[2*i],dataL3K3raw[2*i+1])
            for i in range(len(dataL3K3raw) / 2)]

dataL3K3X = [pt[0] for pt in dataL3K3]
dataL3K3Y = [pt[1]/1000.0 for pt in dataL3K3]


plt.xlabel('Data Points')
plt.ylabel('Time In Seconds')
plt.title('Black-Scholes Performance')
plt.plot(dataL3K3X, dataL3K3Y, label='CPU')
plt.legend(loc=2)
plt.show()


