#!/usr/bin/python
import numpy as np
import parakeet, sys
from parakeet import PAR

def add1(x):
  return x +1

@PAR
def map_add(x):
  return parakeet.map(add1, parakeet.map(parakeet.add, x, x))

def test_map_add_1d():
  print "Testing user-defined map of add operator over 1D data" 
  x = np.random.randint(100, size=1000)
  y = map_add(x)
  y_original = map_add.call_original(x)
  print "Python = %s, Parakeet = %s" % (y_original, y)
  assert np.all(y_original == y)

#def test_map_add_2d():
#  print "Testing user-defined map of add operator over 2D data"
#  x = np.random.randint(100, size=(500, 100))
#  y = map_add(x)
#  y_original = map_add.call_original(x)
#  print "Python = %s, Parakeet = %s" % (y_original, y)
#  assert np.all(y_original == y)


if __name__ == '__main__':
  test_map_add_1d()
  #test_map_add_2d()
  #test_mult()
  #test_mult_rows()

