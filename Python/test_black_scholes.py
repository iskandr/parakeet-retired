#!/usr/bin/python
import math
from parakeet import PAR
import para_libs

@PAR
def CND(x):
  a1 = 0.31938153
  a2 = -0.356563782
  a3 = 1.781477937
  a4 = -1.821255978
  a5 = 1.330274429
  L = para_libs.abs(x)
  K = 1.0 / (1.0 + 0.2316419 * L)
  w = 1.0 - 1.0/math.sqrt(2*3.141592653589793)*math.exp(-1*L*L/2.) * (a1*K +
      a2*K*K + a3*math.pow(K,3) + a4*math.pow(K,4) + a5*math.pow(K,5))
  if x<0:
    w = 1.0-w
  return w

def test_cnd(): 
  print "RUNNING CND"
  for i in range(3):
    x = CND(i)
    y = CND.call_original(i)
    diff= x-y
    same = abs(diff) < 0.00001
    print "[test_cnd] %f == %f: %s (diff = %f)" % (x,y,same, diff)
    assert same

@PAR
def scalar_black_scholes(CallFlag,S,X,T,r,v):
  d1 = (math.log(S/X)+(r+v*v/2.)*T)/(v*math.sqrt(T))
  d2 = d1-v*math.sqrt(T)
  if CallFlag:
    return S*CND(d1)-X*math.exp(-r*T)*CND(d2)
  else:
    return X*math.exp(-r*T)*CND(-d2)-S*CND(-d1)

def test_scalar_black_scholes(): 
  x = scalar_black_scholes(True, 2.0, 2.0, 2.0, 2.0, 2.0)
  y = scalar_black_scholes.call_original(True, 2.0, 2.0, 2.0, 2.0, 2.0)
  diff = x - y 
  same = abs(diff) < 0.00001 
  print "[test_cnd] %f == %f: %s (diff = %f)" % (x, y, same, diff)
  assert same 

@PAR
def black_scholes(CallFlags, S, X, T, r, v):
  return para_libs.map(scalar_black_scholes, CallFlags, S, X, T, r, v)

def test_black_scholes():
  CallFlags = np.array([True, False, True, False])
  A = np.array([2.0, 1.0, 2.0, 1.0])
  x = black_scholes(CallFlags, A, A, A, A, A)
  y = black_scholes.call_original(CallFlags, A, A, A, A, A)
  all_same = np.all(np.abs(x -  y) < 0.00001)
  assert all_same  

if __name__ == '__main__':
  test_cnd()
  test_scalar_black_scholes()
  test_black_scholes()
