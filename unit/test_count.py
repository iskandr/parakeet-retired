from parakeet import PAR

@PAR 
def count_loop(xs, k):
  i = 0
  count = 0 
  while i < len(xs):
    if xs[i] == k:
      count = count + 1
    i = i + 1
  return count 


import numpy as np
xs = np.ones(100000)
xs[0] = 2
xs[1] = 2
xs[100] = 2

def test_count_loop():
  c = count_loop(xs, 2)
  assert c == 3

import parakeet as par 
def count(xs,k):
  return par.sum(xs == k)

def test_count():
  c = count(xs, 2)
  assert c == 3
 
if __name__ == '__main__':
  test_count_loop()
  test_count() 
   
