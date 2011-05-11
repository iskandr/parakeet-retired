

from numpy import * 
from parakeet import GPU 

@GPU 
def f(x, a,c):
   b = a == c
   return x[b]

x = array([1,2,3,4,5,6,7,8])
b = array([True, False, True, False, True, False, True, False])

y = f(x,array([1,2,3]),array([2,2,2]))
