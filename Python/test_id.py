from numpy import *
from parakeet import GPU

@GPU
def id(x):
  return x

multi_test = array([[1,2,3],[4,5,6],[7,8,9]],dtype = int32)

print multi_test
print shape(multi_test)
res =  id(multi_test)
print res
print shape(res)

#import unittest

#class TestIdent(unittest.TestCase):
#  def testMethod(self):
#    self.assertTrue(all(id(multi_test) == multi_test), "identity function broken on 2d data")

