from parakeet import PAR


@PAR
def f():
  return 1

def test_f():
  print "Starting test..."
  assert f() == 1
  print "Done!"

if __name__ == '__main__':
  test_f()
