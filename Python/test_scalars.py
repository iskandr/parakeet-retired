from parakeet import GPU

@GPU
def f(x,y):
    return x + 2 * y - 3

print f(4,1)

