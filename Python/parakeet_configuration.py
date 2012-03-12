from ctypes import *
from parakeet_common import LibPar

def set_vectorize(val):
  LibPar.set_vectorize(val)
