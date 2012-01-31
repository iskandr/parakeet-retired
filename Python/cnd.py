#!/usr/bin/python
import math
from parakeet import PAR
import para_libs

# Cumulative normal distribution
#@PAR
#def stupid(x):
#  a = math.sqrt(2*pi)

@PAR
def CND(X):
  a1 = 0.31938153
  a2 = -0.356563782
  a3 = 1.781477937
  a4 = -1.821255978
  a5 = 1.330274429
  #L = para_libs.abs(X)
  L = 2.0
  K = 1.0 / (1.0 + 0.2316419 * L)
  #w = 1.0 - 1.0/math.sqrt(2*3.141592653589793)*math.exp(-1*L*L/2.) * (a1*K +
  #    a2*K*K + a3*math.pow(K,3) + a4*math.pow(K,4) + a5*math.pow(K,5))
  #if X<0:
  #  w = 1.0-w
  return K

def cpuCND(X):
  a1 = 0.31938153
  a2 = -0.356563782
  a3 = 1.781477937
  a4 = -1.821255978
  a5 = 1.330274429
  L = para_libs.abs(X)
  K = 1.0 / (1.0 + 0.2316419 * L)
  w = K
  w = 1.0 - 1.0/math.sqrt(2*3.141592653589793)*math.exp(-1*L*L/2.) * (a1*K +
      a2*K*K + a3*math.pow(K,3) + a4*math.pow(K,4) + a5*math.pow(K,5))
  if X<0:
      w = 1.0-w
  return w

for i in range(120):
  print "%dth iteration" % i
  print CND(1)# = 0.72574693543

print cpuCND(1)

