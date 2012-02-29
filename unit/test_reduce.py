#!/usr/bin/python
import numpy as np
import parakeet
from parakeet import PAR 

@PAR
def sum_all_elts(x):
  return parakeet.reduce(parakeet.add, x)

def test_sum():
  x = np.random.randn(10,1000)
  parakeet_sum = sum_all_elts(x)
  np_sum = np.sum(x)
  print "Python: %s, Parakeet: %s" % (np_sum, parakeet_sum)
  assert np_sum == parakeet_sum

@PAR
def sum_rows(x):
  return parakeet.reduce(parakeet.add, x, axis=[0])

def test_sum_rows():
  x = np.random.randn(10,1000)
  parakeet_sum_rows = sum_rows(x)
  np_sum_rows = sum(x,axis=0)
  print "Python: %s\n, Parakeet: %s\n" % (np_sum_rows, parakeet_sum_rows)
  assert np.sum( (parakeet_sum_rows - np_sum_rows)**2) <= 0.001

if __name__ == '__main__':
  test_sum()
  #test_sum_rows()

