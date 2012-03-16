#!/usr/bin/python
import numpy as np
import parakeet, sys
from parakeet import PAR 

@PAR
def count_x(x):
  y = 0
  while y < x:
    y = y + 1
  return y 

def test_count_x(): 
  x = count_x(5)
  y = count_x.call_original(5)
  print "Got %d, expected %d" % (x, y)
  assert y == x

@PAR
def sum_to_x(x):
  counter = 0
  y = 0
  while counter < x:
    counter = counter + 1
    y = y + counter
  return y

def test_sum_to_x():
  y = count_x(-3)
  print "Expected 0, got", y
  assert 0 == y
  x = sum_to_x(5)
  y = sum_to_x.call_original(5)
  print "Expected %d, got %d" % (y, x)
  assert 15 == x

@PAR
def array_pow(x, b, n):
  z = x
  i = 0
  while i < n:
    z = z * b
  return z

def test_array_pow():
  x = np.array(range(100), dtype=np.int32)
  y = array_pow(x, 3, 5)
  z = array_pow.call_original(x, 3, 5)
  assert np.all(z == y)
  print "Array pow OK"

if __name__ == '__main__':
  test_count_x()
  test_sum_to_x()
  test_array_pow()

