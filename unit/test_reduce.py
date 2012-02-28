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
  #x = np.random.randint(0,100,1000)
  x = np.arange(1000, dtype='float')
  parakeet_sum = sum(x)
  np_sum = np.sum(x)
  print "Python: %s, Parakeet: %s" % (np_sum, parakeet_sum)
  assert np_sum == np_sum 

if __name__ == '__main__':
  test_sum()

