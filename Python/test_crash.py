#!/usr/bin/python
from numpy import *
from parakeet import PAR
import sys


@PAR 
def always_crash():
  assert 1 == 2

@PAR 
def const2():
  return 2 

@PAR 
def always_crash2():
  assert 1 == 2

def test_const2():
  assert 2 == const2()

if __name__ == '__main__':
  print "Running tests"
  test_const2()
