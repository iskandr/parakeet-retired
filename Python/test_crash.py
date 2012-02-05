#!/usr/bin/python
from numpy import *
from parakeet import PAR
import sys


@PAR 
def const2():
  return 2 

@PAR
def const3():
  return 3

@PAR 
def const4(): 
  return 4

@PAR 
def const5(): 
  return 5

def test_const2():
  for i in xrange(100): 
    assert 2 == const2()

if __name__ == '__main__':
  print "Running tests"
  test_const2()
