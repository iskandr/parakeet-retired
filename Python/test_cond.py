from numpy import *
from parakeet import PAR
import sys

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

if __name__ == '__main__':
  test_if()
  test_complex_if()
