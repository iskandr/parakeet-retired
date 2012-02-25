import numpy as np
import unit
from unit import parakeet
from parakeet import PAR 

def add2(x, y):
  return x + y

@PAR
def sum(x):
  return parakeet.reduce(add2, x)

def test_sum():
  x = np.random.randint(0,100,1000)
  parakeet_sum = sum(x)
  np_sum = np.sum(x)
  print "Python: %s, Parakeet: %s" % (python_sum, parakeet_sum)
  assert parakeet_sum == np_sum 

if __name__ == '__main__':
  test_sum()

