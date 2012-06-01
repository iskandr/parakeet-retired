from numpy import array, argmin, arange
from functools import *
import parakeet
import parakeet as par
import numpy as np

def sqr_dist(x,y):
  return par.sum((x-y) * (x-y), axis=0)

def minidx(C,x):
  return par.argmin(parakeet.map(sqr_dist, C, fixed=x))

def calc_centroid(X,a,i):
  return par.mean(X[a == i], axis=0)

def kmeans(X,assign,k):
  C = par.map(calc_centroid, arange(k), fixed=(X, assign))
  converged = False
  while not converged:
    lastAssign = assign
    assign = par.map(minidx, X, fixed=C)
    converged = np.all(assign == lastAssign)
    C = parakeet.map(calc_centroid, arange(k), fixed=(X, assign))
  return C

X = array([1,2,3,4,5,6,7,8,9])
assign = array([1,0,1,1,1,0,0,0,1])
k = 2
print kmeans(X,assign,k)

