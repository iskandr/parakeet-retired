#!/usr/bin/python
import numpy as np
import parakeet, sys
from parakeet import PAR

parakeet.set_vectorize(False)

@PAR 
def implicit_map(x):
  return x * 3

def test_implicit_map():
  print "Testing implicit maps" 
  x = np.array(range(50))
  y = implicit_map(x)
  y_original = x * 3
  print "Python = %s, Parakeet = %s" % (y_original, y)
  assert np.all(y_original == y)

@PAR
def map_add(x):
  return parakeet.map(parakeet.add, x, x)

def test_map_add_1d():
  print "Testing user-defined map of add operator over 1D data" 
  x = np.random.randint(100, size=1000)
  y = map_add(x)
  y_original = x + x 
  print "Python = %s, Parakeet = %s" % (y_original, y)
  assert np.all(y_original == y)

def test_map_add_2d():
  print "Testing user-defined map of add operator over 2D data"
  x = np.random.randint(100, size=(500, 100))
  y = map_add(x)
  y_original = x+x
  print "Python = %s, Parakeet = %s" % (y_original, y)
  assert np.all(y_original == y)

@PAR
def mult_vec_scalar(x,y):
  return parakeet.map(parakeet.mult, x, y)

def test_mult_vec_scalar():
  x = np.array([10, 0, 10, 20])
  y = 3 
  z = mult_vec_scalar(x, 3)
  z_original = x * 3
  print "Python %s, Parakeet %s" % (z_original, z)
  assert np.all(z_original == z) 

def array_eq(x,y):
  np.all(np.abs(x - y) < 0.000001)

@PAR 
def map_mult_rows(mat, row):
  return parakeet.map(parakeet.mult, mat, row, axis=[0])

def test_mult_rows():
  x = np.random.randn(1000, 100)
  y = np.random.randn(100)
  z = map_mult_rows(x,y)
  z_o = x * y 
  print "Python %s\nParakeet: %s" % (z_o, z)
  assert array_eq(z, z_o)

if __name__ == '__main__':
  test_implicit_map()
  test_map_add_1d()
  test_map_add_2d()
  test_mult_vec_scalar()
  test_mult_rows()

