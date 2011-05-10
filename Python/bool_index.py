

from numpy import * 
from parakeet import GPU 

@GPU 
def f(x, b):
   return x[b]

x = array([1.0,2,3,4,5,6,7,8])
b = array([True, False, True, False, True, False, True, False])

y = f(x,b)
