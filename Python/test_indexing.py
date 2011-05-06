from numpy import *  
from parakeet import GPU

@GPU
def fourthElement(x):
  return x[3]

arrtest = array([1,2,3,4,5,6,7,8,9,10],dtype = int32)
scaltest = 8

import unittest

class MyTest(unittest.TestCase):

    def testMethod(self):
        self.assertTrue(fourthElement(arrtest) == 4, "fourthElement not working")

if __name__ == '__main__':
    unittest.main()