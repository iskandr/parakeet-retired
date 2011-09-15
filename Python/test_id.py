from numpy import *
from parakeet import GPU

@GPU
def identity(x):
  return x

multi_test = reshape(arange(12, dtype=int32), (3,4))
#multi_test = arange(12, dtype=int32)

print "Input:", multi_test
print "Input Shape:", shape(multi_test)
print "Input Type:", multi_test.dtype
res =  identity(multi_test)
print "Output: ", res
print "Output Shape:", shape(res)
print "Output Type:", res.dtype
#import unittest

#class TestIdent(unittest.TestCase):
#  def testMethod(self):
#    self.assertTrue(all(id(multi_test) == multi_test), "identity function broken on 2d data")

