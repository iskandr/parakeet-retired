#!/usr/bin/python
import numpy as np
import parakeet, sys
from parakeet import PAR

@PAR 
def implicit_map(x):
  return x * 3

def test_implicit_map():
  print "Testing implicit maps" 
  x = np.array(range(50))
  y = implicit_map(x)
  y_original = implicit_map.call_original(x)
  print "Python = %s, Parakeet = %s" % (y_original, y)
  assert np.all(y_original == y)

@PAR
def map_add(x):
  return parakeet.map(parakeet.add, x, x)

def test_map_add_1d():
  print "Testing user-defined map of add operator over 1D data" 
  x = np.random.randint(100, size=1000)
  y = map_add(x)
  y_original = map_add.call_original(x)
  print "Python = %s, Parakeet = %s" % (y_original, y)
  assert np.all(y_original == y)

def test_map_add_2d():
  print "Testing user-defined map of add operator over 2D data"
  x = np.random.randint(100, size=(500, 100))
  y = map_add(x)
  y_original = map_add.call_original(x)
  print "Python = %s, Parakeet = %s" % (y_original, y)
  assert np.all(y_original == y)

#@PAR
def map_mult(x,y):
  return parakeet.map(parakeet.mult, x, y)

def test_mult():
  x = np.array([10, 0, 10, 20])
  z = map_mult(x, fixed=[3])
  z_original = map_mult.call_original(x,y)
  print "Python %s, Parakeet %s" % (y_original, y)
  assert np.all(y_original == y) 

#@PAR 
def map_mult_rows(mat, row):
  return parakeet.map(parakeet.mult, mat, row, axis=[0])

def test_mult_rows():
  x = np.random.randn(1000, 100)
  y = np.random.randn(100)
  z = map_mult_rows(x,y)
  z_o = map_mult_rows.call_original(x,y)
  print "Python %s\nParakeet: %s" % (z_o, z)
  assert np.all(z == z_o)

if __name__ == '__main__':
  test_implicit_map()
  test_map_add_1d()
  test_map_add_2d()
  #test_mult()
  #test_mult_rows()

