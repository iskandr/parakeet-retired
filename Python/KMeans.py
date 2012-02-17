from numpy import array, argmin, arange
from functools import *
import parakeet 
from parakeet import PAR

def sqr_dist(x,y):
  return parakeet.sum((x-y) * (x-y))

#@PAR
def minidx(C,x):
  return parakeet.argmin(parakeet.map(sqr_dist,C,fixed=[x]))

def calc_centroid(X,a,i):
  return parakeet.mean(X[a == i], 0)

@PAR 
def kmeans(X,assign,k):
  C = parakeet.map(calc_centroid, arange(k), fixed=[X, assign])
  converged = False
  while not converged:
    lastAssign = assign
    assign = parakeet.map(minidx, X, fixed=[C])
    converged = parakeet.all(assign == lastAssign)
    C = parakeet.map(calc_centroid, arange(k), fixed=[X, assign])
  return C

X = array([1,2,3,4,5,6,7,8,9])
assign = array([1,0,1,1,1,0,0,0,1])
k = 2
print kmeans(X,assign,k)

