import numpy as np
import sys, os
sys.path.append(os.getenv("PARAKEET_PATH") + "/Python")
import parakeet
from parakeet import PAR
from performance import *

@PAR
def map_add(x):
  return parakeet.map(parakeet.add, x, x)

def np_add(x):
  return x + x

def test_add():
  x = np.random.randint(100, size=(10000, 5000))
  speedup = run(map_add, np_add, [x])
  print_result("2D Matrix addition", speedup)

