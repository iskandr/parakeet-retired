#!/usr/bin/python
from numpy import array, argmin, arange
from functools import *
import parakeet
from parakeet import PAR
import numpy as np

def sqr_dist(x,y):
  return np.sum((x-y) * (x-y), 0)

def calc_assign(C, x):
  return np.argmin(parakeet.map(sqr_dist, C, fixed=[x]))

@PAR
def calc_assigns(X, C):
  return parakeet.map(calc_assign, X, fixed=[C])

def calc_centroid(X, a, i):
  return np.mean(X[a == i], 0)

@PAR
def calc_centroids(X, assign, k):
  return parakeet.map(calc_centroid, np.arange(k), fixed=[X, assign])

def test_ccs():
  X = np.random.random(size=(1000, 100))
  assign = np.random.randint(2, size=1000)
  k = 2
  C = calc_centroids(X, assign, k)
  new_assign = calc_assigns(X, C)
  new_C = calc_centroids(X, new_assign, k)

if __name__ == '__main__':
  test_ccs()

