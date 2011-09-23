from numpy import *  
from parakeet import GPU
import test_multidiminput

@GPU
def fourthElement(x):
  return x[3]

@GPU
def arrElements(x,y):
  return x[y]

#@GPU
def calc_centroid(X,a,i): 
    return mean(X[a == i])

arr_test = array([1,2,3,4,5,6,7,8,9,10],dtype = int32)
arr_index = array([3,5,7],dtype = int32)
arr_index_ans = array([4,6,8],dtype = int32)
arr_a = array([0,1,3,1,2,1,3,3,5,6],dtype = int32)
i = 1
scal_test = 8

import unittest

class MyTest(unittest.TestCase):

  def testMethod(self):
    self.assertTrue(fourthElement(arr_test)==4,"fourthElement not working")
    self.assertTrue(all(arrElements(arr_test,arr_index) == arr_index_ans), "arrElements not working")
    self.assertTrue(calc_centroid(arr_test,arr_a,i)==4,"calc_centroid not working")
#if __name__ == '__main__':
#  unittest.main()
  
#print arrElements(arr_test,arr_index)
