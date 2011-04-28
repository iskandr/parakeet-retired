from numpy import * 
from functools import * 

def sqr_dist(x,y): 
    return sum((x-y) * (x-y))

def minidx(C,x): 
    return argmin(map(partial(sqr_dist, x), C))
    
def calc_centroid(X,a,i): 
    return mean(X[a == i], 0)

def kmeans(X,assign,k):
    C = map(partial(calc_centroid, X, assign), arange(k))
    converged = False
    while not converged:
        lastAssign = assign
        assign = map(partial(minidx, C), X)
        converged = all(assign == lastAssign)
        C = map(partial(calc_centroid, X, assign), arange(k))
    return C