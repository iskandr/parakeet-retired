#!/usr/bin/python
import numpy as np 
import math, parakeet
from parakeet import PAR 

parakeet.set_vectorize(False)

@PAR
def CND(x):
  a1 = 0.31938153
  a2 = -0.356563782
  a3 = 1.781477937
  a4 = -1.821255978
  a5 = 1.330274429
  L = x #parakeet.abs(x)
  K = 1.0 / (1.0 + 0.2316419 * L)
  w = 1.0 - 1.0/math.sqrt(2*3.141592653589793)*math.exp(-1*L*L/2.) * (a1*K +
      a2*K*K + a3*math.pow(K,3) + a4*math.pow(K,4) + a5*math.pow(K,5))
  #if x<0:
  #  w = 1.0-w
  return w

#@PAR
def map_cnd(x):
  return parakeet.map(CND, x)

def test_cnd(): 
  print "RUNNING CND"
  x = np.arange(10000, dtype='float32')
  y = map_cnd(x)
  z = np.array([CND.call_original(xi) for xi in x])
  print "Parakeet: ", y
  print "Python: ", z 
  assert np.sum(abs(y-z)) < 0.00001


if __name__ == '__main__':
  test_cnd()
