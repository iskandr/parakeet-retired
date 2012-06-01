#!/usr/bin/python
import numpy as np 
import math, parakeet
from parakeet import PAR 

parakeet.set_vectorize(False)
parakeet.set_multithreading(False)

def CND(x):
  a1 = 0.31938153
  a2 = -0.356563782
  a3 = 1.781477937
  a4 = -1.821255978
  a5 = 1.330274429
  L = abs(x)
  K = 1.0 / (1.0 + 0.2316419 * L)
  w = 1.0 - 1.0/math.sqrt(2*3.141592653589793)* math.exp(-1*L*L/2.) * (a1*K +
      a2*K*K + a3*K*K*K + a4*K*K*K*K + a5*K*K*K*K*K)
  if x<0:
    w = 1.0-w
  return w

def test_cnd(): 
  print "RUNNING CND"
  fast_cnd = PAR(CND)
  for i in range(10):
    x = fast_cnd(i)
    y = CND(i)
    diff= x-y
    same = abs(diff) < 0.00001
    print "[test_cnd] %d) %f == %f: %s (diff = %f)" % (i, x,y,same, diff)
    assert same

def scalar_black_scholes(CallFlag,S,X,T,r,v):
  d1 = (math.log(S/X)+(r+v*v/2.)*T)/(v*math.sqrt(T))
  d2 = d1-v*math.sqrt(T)
  z = math.exp(-r*T) * X
  if CallFlag:
    return S*CND(d1) - z*CND(d2)
  else:
    return z*CND(-d2) - S*CND(-d1)

def test_scalar_black_scholes(): 
  x1 = (False, 10.0, 10.0, 2.0, 2.0, 2.0)
  x2 = (True, 10.0, 10.0, 2.0, 2.0, 2.0)
  fast_scalar_black_scholes = PAR(scalar_black_scholes)
  y1 = fast_scalar_black_scholes(*x1)
  y2 = fast_scalar_black_scholes(*x2)
  z1 = scalar_black_scholes(*x1)
  z2 = scalar_black_scholes(*x2)
  print "Test scalar black scholes"
  print "[False] Parakeet: %s, Python: %s" % (y1, z1)
  print "[True] Parakeet: %s, Python: %s" % (y2, z2) 
  d1 = y1 - z1
  same1 = abs(d1) < 0.00001 
  print "[test_scalar_black_scholes] \
     %f == %f: %s (diff = %f)" % (y1, z1, same1, d1)
  assert same1 
  d2 = y2 - z2 
  same2 = abs(d2) < 0.00001
  print "%f == %f : %s (diff = %f)" % (y2, z2, same2, d2)
  assert same2

def slow_black_scholes(CallFlags, S, X, T, r, v):
  result = [] 
  for i, c in enumerate(CallFlags): 
    result.append(scalar_black_scholes(c, S[i], X[i], T[i], r[i], v[i]))  
  return np.array(result)
 
def test_black_scholes():
  CallFlags = np.array([True, False, True, False])
  A = np.array([2.0, 1.0, 2.0, 1.0])
  inputs = (CallFlags, A, A, A, A, A)
  x = parakeet.map(scalar_black_scholes, *inputs) 
  y = slow_black_scholes(*inputs)
  print "BLACK SCHOLES RESULTS Parakeet = %s\n Python = %s" % (x,y)
  assert np.all(np.abs(x -  y) < 0.00001)

if __name__ == '__main__':
  test_cnd()
  test_scalar_black_scholes()
  test_black_scholes()

