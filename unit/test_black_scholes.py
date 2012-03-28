#!/usr/bin/python
import numpy as np 
import math, parakeet
from parakeet import PAR 

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
    print "[test_cnd] %f == %f: %s (diff = %f)" % (x,y,same, diff)
    assert same


def scalar_black_scholes(CallFlag,S,X,T,r,v):
  d1 = (math.log(S/X)+(r+v*v/2.)*T)/(v*math.sqrt(T))
  d2 = d1-v*math.sqrt(T)
  if CallFlag:
    return S*CND(d1)-X*math.exp(-r*T)*CND(d2)
  else:
    return 0#X* math.exp(-r*T)*CND(-d2)-S*CND(-d1)

def test_scalar_black_scholes(): 
  inputs = (False, 10.0, 10.0, 2.0, 2.0, 2.0)
  fast_scalar_black_scholes = PAR(scalar_black_scholes)
  x = fast_scalar_black_scholes(*inputs)
  y = scalar_black_scholes(*inputs)
  diff = x - y 
  print "Test scalar black scholes, Parakeet: %s, Python: %s" % (x, y)
  same = abs(diff) < 0.00001 
  print "[test_cnd] %f == %f: %s (diff = %f)" % (x, y, same, diff)
  assert same 

def black_scholes(CallFlags, S, X, T, r, v):
  return parakeet.map(scalar_black_scholes, CallFlags, S, X, T, r, v)

def test_black_scholes():
  CallFlags = np.array([True, False, True, False])
  A = np.array([2.0, 1.0, 2.0, 1.0])
  fast_black_scholes = PAR(black_scholes)
  inputs = (CallFlags, A, A, A, A, A)
  x = fast_black_scholes(*inputs)
  y = black_scholes(*inputs)
  print "BLACK SCHOLES RESULTS Parakeet = %s\n Python = %s" % (x,y)
  assert np.all(np.abs(x -  y) < 0.00001)

if __name__ == '__main__':
  test_cnd()
  test_scalar_black_scholes()
  test_black_scholes()
