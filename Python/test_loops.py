#!/usr/bin/python
import sys
from parakeet import PAR 

@PAR
def count_x(x):
  y = 0
  while y < x:
    y = y + 1
  return y 

def test_count_x(): 
  x = count_x(5)
  print "Expected 5, got", x
  assert 5 == x

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
  print "Expected 15, got", x
  assert 15 == x

if __name__ == '__main__':
  test_count_x()
  test_sum_to_x()
