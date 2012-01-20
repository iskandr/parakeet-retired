from parakeet import PAR

@PAR
def f(x,y):
    return x * y + x 

print f(4,1)

import unittest

class MyTest(unittest.TestCase):

    def testMethod(self):
        x = f(4, 2)
        print "f(4,2) =", x
        self.assertTrue(x==12, "f(4,2) != 12")
        x = f(4.0,1.0)
        print "f(4.0,1.0) =", x 
        self.assertTrue(x==8.0,"f(4.0,1.0) != 8.0")
        x = f(4,4)
        print "f(4,4) =", x 
        self.assertTrue(x==20,"f(4,4) != 9")
#        self.assertTrue(abs(f(1.3,2.5)-3.3) < .1,"f(1.3,2.5) != 3.3")

if __name__ == '__main__':
    unittest.main()

