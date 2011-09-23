from numpy import * 
#from functools import * 
from parakeet import GPU


#@GPU
def sqr_dist(x,y): 
    
    return sum((x-y) * (x-y))

#@GPU
def minidx(C,x): 
    return mean(map(partial(sqr_dist, x), C))

x = array([1,2,3,4,5,6,7,8,9,10],dtype = int32)
y = array([3,1,4,1,5,9,2,6,5,3],dtype = int32)

#print sqr_dist(x,2)
#print minidx(x,y)

@GPU
def add2(x,y):
  return x + y

@GPU
def whileTestScalar(x):
  while x > 0:
    x = x - add2(1,2)
  return x

print whileTestScalar(1.2)

def whileTest(x,y):
  converged = False
  while not converged:
    z = sqr_dist(x,y)
    converged = True
  return z
#  while x > 0:
#    x = x - 1
#  return x


