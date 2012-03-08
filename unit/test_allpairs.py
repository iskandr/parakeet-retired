import numpy as np 
import parakeet


@parakeet.PAR
def f(x):
  return parakeet.allpairs(np.add, x, x)

def test_f():
  x = np.arange(50)
  y = f(x)
  z = np.add.outer(x,x)
  print "Parakeet shape = %s, value = %s" % (y.shape, y)  
  print "Python shape = %s, value = %s " % (z.shape,  z) 

if __name__ == '__main__':
  test_f()

