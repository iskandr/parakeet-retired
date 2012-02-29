#!/usr/bin/python
from numpy import *
import parakeet, sys
from parakeet import PAR

@PAR
def simple_if(x):
  a = 0
  if x:
    a = 1
  else:
    a = 0
  return a

@PAR
def complex_if(x):
  a = 0
  if x == 3:
    a = 1
  else:
    a = 2
  return a

@PAR
def float_if(x):
  a = 0.
  if x == 2.5:
    a = 2.5
  else:
    a = 1.5
  return a

def test_if():
  print "Testing If"
  sys.stdout.flush()
  x = simple_if(1)
  print "Expected 1, got", x
  assert 1 == x
  y = simple_if(0)
  print "Expected 0, got", y
  assert 0 == y

def test_complex_if():
  x_complex = complex_if(3)
  print "Expected 1, got", x_complex
  assert 1 == x_complex
  y_complex = complex_if(1)
  print "Expected 2, got", y_complex
  assert 2 == y_complex

def test_float_if():
  x_float = float_if(2.5)
  print "Expected 2.5, got", x_float
  assert 2.5 == x_float
  y_float = float_if(2.)
  print "Expected 1.5, got", y_float
  assert 1.5 == y_float

if __name__ == '__main__':
  test_if()
  test_complex_if()
  test_float_if()
