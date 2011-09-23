from parakeet import GPU

@GPU
def f(x,y):
    return x + 2 * y - 3

print f(4,1)

import unittest

class MyTest(unittest.TestCase):

    def testMethod(self):
        self.assertTrue(f(4,1)==3,"f(4,1) != 3")
        self.assertTrue(f(4,4)==9,"f(4,4) != 9")
        self.assertTrue(abs(f(1.3,2.5)-3.3) < .1,"f(1.3,2.5) != 3.3")

if __name__ == '__main__':
    unittest.main()

