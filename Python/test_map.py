#!/usr/bin/python
import numpy as np
import parakeet
from parakeet import PAR
import sys

@PAR 
def implicit_map(x):
  return x * 3

def test_implicit_map():
  print "Testing implicit maps" 
  x = np.array([1,2,3,4])
  y = implicit_map(x)
  y_original = implicit_map.call_original(x)
  print "Python = %s, Parakeet = %s" % (y_original, y)
  assert np.all(y_original == y)


@PAR
def custom_map_add(x):
  return parakeet.map(parakeet.add, x, x)

def test_custom_map_add():
  print "Testing user-defined map of add operator" 
  x = np.array([-10, 0, 10, 20])
  y = custom_map_add(x)
  y_original = custom_map_add.call_original(x)
  print "Python = %s, Parakeet = %s" % (y_original, y)
  assert np.all(y_original == y)


if __name__ == '__main__':
  test_implicit_map()
  test_custom_map_add()

