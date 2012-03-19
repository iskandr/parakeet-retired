#!/usr/bin/python
import numpy as np
import parakeet, sys
from parakeet import PAR

parakeet.set_vectorize(False)

@PAR
def array_if(x, b):
  z = x
  if b:
    z = z * 3
  return z

def test_array_if():
  print "Testing array if"
  x = np.array(range(100), dtype=np.int32)
  y = array_if(x, True)
  y_original = array_if.call_original(x, True)
  print "Python = %s, Parakeet = %s" % (y_original, y)
  assert np.all(y_original == y)

@PAR 
def if_with_bool(x, b):
  z = x
  if b:
    z = x * 3
  else:
    z = x * 7
  return z

def test_with_bool():
  print "Testing if with boolean argument"
  x = np.array(range(100), dtype=np.int32)
  y = if_with_bool(x, True)
  y_original = if_with_bool.call_original(x, True)
  print "Python = %s, Parakeet = %s" % (y_original, y)
  assert np.all(y_original == y)

@PAR
def if_true(x):
  z = x
  if True:
    z = x * 3
  else:
    z = x * 7
  return z

def test_true():
  print "Testing if with True argument"
  x = np.array(range(100), dtype=np.int32)
  y = if_true(x)
  y_original = if_true.call_original(x)
  print "Python = %s, Parakeet = %s" % (y_original, y)
  assert np.all(y_original == y)

@PAR
def if_return(b):
  x = -1 
  if b:
    return 1
  return x 
    

def test_if_return():
  a = if_return(True)
  print "Python = 1, Parakeet = ", a
  assert a == 1
  b = if_return(False)
  print "Python = -1, Parakeet = ", b
  assert b == -1


if __name__ == '__main__':
  test_array_if()
  test_with_bool()
  test_true()
  test_if_return()
