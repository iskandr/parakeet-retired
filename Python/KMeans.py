from numpy import array, argmin, arange
from functools import *
import para_libs
#from parakeet import PAR


def sqr_dist(x,y):
    return para_libs.sum((x-y) * (x-y))

def minidx(C,x):
    return argmin(para_libs.map(sqr_dist,array(C),fixed=[x]))

def calc_centroid(X,a,i):
    return para_libs.mean(X[a == i], 0)

def kmeans(X,assign,k):
    C = para_libs.map(calc_centroid, arange(k), fixed=[X, assign])
    converged = False
    while not converged:
        lastAssign = assign
        assign = para_libs.map(minidx, X, fixed=[C])
        converged = all(assign == lastAssign)
        C = para_libs.map(calc_centroid, arange(k), fixed=[X, assign])
    return C

X = array([1,2,3,4,5,6,7,8,9])
assign = array([1,0,1,1,1,0,0,0,1])
k = 2
print kmeans(X,assign,k)