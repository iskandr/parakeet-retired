import numpy as np
import parakeet as par
from math import pi, cos

idxs = [[(x, y) for y in range(8)] for x in range(8)]
cos_dict = [[cos((pi/8)*(x+0.5)*y) for y in range(8)] for x in range(8)]

def alpha(n):
  if n == 0:
    return 0.353553390593
  else:
    return .5

def dct_step(u, v, x, y, g):
  return alpha(u)*alpha(v)*g*cos_dict[x][u]*cos_dict[y][v]

def add2(x, y):
  return x + y

def dct_val(I, g_idxs):
  return par.reduce(add2,
                    par.map(dct_step, X, fixed=[g_idxs[0], g_idxs[1],
                                                par.idxs[0], par.idxs[1]])

def dct(I):
  return par.map(dct_val, idxs, fixed=[I])

def dcts(Is):
  return par.map(dct, Is)

