#!/usr/bin/python
import parakeet
from parakeet import PAR
import numpy as np

parakeet.set_vectorize(False)
parakeet.set_multithreading(False)


@PAR
def addReduce(x):
  return np.add.reduce(x)


def test_reduce_all():
  x = np.array(([1,2,3],[4,5,6]))
  out = addReduce(x)
  e_out = np.array(([5,7,9]))
  print "Expected ", e_out, " got: ", out
  assert np.all(out == e_out)

@PAR
def addReduceAxis(x):
  return np.add.reduce(x,1)

def test_reduce_axis():
  x = np.array(([1,2,3],[4,5,6]))
  out = addReduceAxis(x)
  e_out = np.array(([6,15]))
  print "Expected ", e_out, " got: ", out
  assert np.all(out == e_out)

#@PAR
#def addAccumulate(x):
#  return np.add.accumulate(x)

#@PAR
#def addAccumulateAxis(x):
#  return np.add.accumulate(x,1)
"""
def test_reduce():
  x = np.array(([1,2,3],[4,5,6]))
  out = addAccumulate(x)
  e_out = np.array([[1,2,3],[5,7,9]])
  print "Expected ", e_out, " got: ", out
  assert np.all(out == e_out)
  out = addAccumulateAxis(x)
  e_out = np.array([[1,3,6],[4,9,15]])
  print "Expected ", e_out, " got: ", out
  assert np.all(out == e_out)
"""
if __name__ == '__main__':
  test_reduce_all()
  test_reduce_axis()
