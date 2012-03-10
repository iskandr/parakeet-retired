#!/usr/bin/python
import numpy as np 
import parakeet

@parakeet.PAR
def allpairs_add(x):
  return parakeet.allpairs(np.add, x, x)

def test_add1d():
  x = np.arange(16)
  y = allpairs_add(x)
  z = np.add.outer(x,x)
  print "Parakeet shape = %s, value = %s" % (y.shape, y)  
  print "Python shape = %s, value = %s " % (z.shape,  z) 
  assert np.all(y == z)

def allpairs_left_helper(x,y):
  return x
 
@parakeet.PAR
def allpairs_left2d(X, Y):
  return parakeet.allpairs(allpairs_left_helper, X, Y, axis=0)
  
def test_allpairs_left2d():
  x = np.array([[1,2,3], [4,5,6], [7,8,9], [10, 11, 12]])
  x1 = x * 10 
  y = allpairs_left2d(x, x1)
  n = x.shape[0] 
  m = x.shape[1]   
  z = np.zeros((n,n,m), dtype='int')
  for i in xrange(n):
    for k in xrange(m):  
      z[i,:,k] = x[i, k]
  print "Parakeet shape = %s, value = %s" % (y.shape, y)  
  print "Python shape = %s, value = %s " % (z.shape,  z) 
  assert np.all(y == z)

if __name__ == '__main__':
  test_add1d()
  test_allpairs_left2d()

