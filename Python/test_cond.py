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
  x = complex_if(3)
  print "Expected 1, got", x
  assert 1 == x
  y = complex_if(1)
  print "Expected 0, got", y
  assert 0 == y

@PAR
def count_x(x):
  y = 0
  while y < x:
    y = y + 1
  return x

@PAR
def sum_to_x(x):
  counter = 0
  y = 0
  while counter < x:
    counter = counter + 1
    y = y + counter
  return y

def test_while():
  print "Testing While"
  sys.stdout.flush()
  x = count_x(5)
  print "Expected 5, got", x
  assert 5 == x
  y = count_x(-3)
  print "Expected 0, got", y
  assert 0 == y
  x = sum_to_x(5)
  print "Expected 15, got", x
  assert 15 == x
  y = sum_to_x(5)
  print "Expected 0, got", y
  assert 0 == y

if __name__ == '__main__':
  test_if()
  test_while()
