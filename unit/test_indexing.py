#!/usr/bin/python
from numpy import *  
import parakeet
from parakeet import PAR

@PAR
def fourthElement(x):
  return x[3]

@PAR
def secondThirdElement(x):
  return x[1,2]


def test_const_index():
  arr_test = array([1,2,3,4,5,6,7,8,9,10], dtype = int64)
  print "Testing constant indexing"
  print "Input: ", arr_test
  res = fourthElement(arr_test)
  print "Expected %d, got %d" % (arr_test[3], res)
  assert res == arr_test[3]

def test_multi_index():
  arr_test = array([[1,2,3,4],[5,6,7,8],[10,10,10,12]], dtype = int64)
  print "Testing indexing by 2 constants"
  print "Input: ", arr_test
  res = secondThirdElement(arr_test)
  print "Expected %d, got %d" % (arr_test[1][2], res)
  assert res == arr_test[1][2]

@PAR
def arrElements(x,y):
  return x[y]

def test_arr_index():
  arr_test = array([1,2,3,4,5,6,7,8,9,10], dtype = int64)
  arr_index = array([3,5,7], dtype = int32)
  arr_index_ans = array([4,6,8], dtype = int32)
  print "Testing indexing by arrays"
  print "Input: ", arr_test
  res = arrElements(arr_test, arr_index)
  print "Expected: ", arr_index_ans
  print "Got: ", res
  assert ndarray.__eq__(res, arr_index_ans).all()

if __name__ == '__main__':
  test_const_index()
  test_multi_index()
  test_arr_index()

