from parakeet import PAR

@PAR
def f(x,y):
    return x * y + x 

@PAR 
def thrice(x):
  return x + x + x

def test_thrice():
  x = thrice(2)
  print "thrice(2) = ", x
  assert (x == 6)

@PAR
def add_three_of_two_args(x,y):
  return x + x + y

def test_add_3_of_2_args():
  x = add_three_of_two_args(1,2)
  print x
  assert (x == 4)

@PAR 
def mult_three_args(x,y,z):
  return  1 * 1 * x

def test_mult_three_args():
  x = mult_three_args(2,4,6)
  print "mult_three_args(2,4,6) =", x
  assert (x == 48)

@PAR 
def add_all_args2(x,y):
  return x+y

@PAR
def add_all_args3(x,y,z): 
  return x+y+z 

@PAR
def  h(x,y,z):
  return y

@PAR 
def add2(x):
  return x + 2

@PAR
def add1_float(x):
  return x + 1.0 

def test_add_const():
  x = add2(1)
  print "add2(1) =", x
  assert (x == 3)
  x = add1_float(1.0)
  print "add1_float(1.0) =", x
  assert (x== 2.0)

 
@PAR 
def double_add(x): 
  return x + x

@PAR 
def double_mult(x):
  return 2 * x

def test_double():
  x = double_add(10)
  print "double(10) =", x
  assert (x == 20)
  x = double_mult(10)
  print "double_mult(10) = ", x
  assert (x==20)
  x = double_add(4.0)
  print "double(4.0) =", x
  assert (x == 8.0)


def test_one_of_three():
  x = h(1,2,3)
  print "h(1,2,3) =", x
  assert (x == 2)

def test_add_all_args():
  x = add_all_args2(14,2)
  print "add_all_args2(14,2) =", x
  assert (x == 16)
  x = add_all_args3(1,2,4)
  print "add_all_args3(1,2,4) = ", x 
  assert (x == 7)

def test_math_ops():
  x = f(4, 2)
  print "f(4,2) =", x
  assert (x==12)
  x = f(4.0,1.0)
  print "f(4.0,1.0) =", x 
  assert (x==8.0)
  x = f(4,4)
  print "f(4,4) =", x 
  assert (x==20)

if __name__ == '__main__':
    unittest.main()

