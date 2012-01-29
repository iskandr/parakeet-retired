import math
from parakeet import PAR
import para_libs

# Cumulative normal distribution
#@PAR
#def stupid(x):
#  a = math.sqrt(2*pi)

@PAR
def CND(X):
    a1 = 0.31938153
    a2 = -0.356563782
    a3 = 1.781477937
    a4 = -1.821255978
    a5 = 1.330274429
    L = para_libs.abs(X)
    K = 1.0 / (1.0 + 0.2316419 * L)
    w = 1.0 - 1.0/math.sqrt(2*3.141592653589793)*math.exp(-1*L*L/2.) * (a1*K +
        a2*K*K + a3*math.pow(K,3) + a4*math.pow(K,4) + a5*math.pow(K,5))
    if X<0:
        w = 1.0-w
    return w

# Black Sholes Function
def BlackSholes(CallPutFlag,S,X,T,r,v):
    d1 = (math.log(S/X)+(r+v*v/2.)*T)/(v*math.sqrt(T))
    d2 = d1-v*math.sqrt(T)
    if CallPutFlag=='c':
        return S*CND(d1)-X*math.exp(-r*T)*CND(d2)
    else:
        return X*math.exp(-r*T)*CND(-d2)-S*CND(-d1)

for i in range(13):
  print CND(1)# = 0.72574693543
