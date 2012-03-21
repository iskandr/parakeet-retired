#!/usr/bin/python
import parakeet
from parakeet import PAR
import numpy as np

x = 3

@PAR
def get_simple_global():
  return x+1

def test_get_simple_global():
  global x
  out = get_simple_global()
  print "Expected", x+1, " got: ", out
  assert x+1 == out
  x = 5
  out = get_simple_global()
  print "Expected", x+1, " got: ", out
  assert x+1 == out

@PAR
def add_pi(x):
  return x + np.pi

def test_add_pi():
  out = add_pi(0)
  print "Expected",np.pi," got: ", out
  assert out == np.pi

if __name__ == '__main__':
  test_get_simple_global()
  test_add_pi()