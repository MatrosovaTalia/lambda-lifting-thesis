def sum(n):
  def sum2(x):
    return (x + n)
  if n < 1:
    a = 1
  else:
    a = sum2(sum(n - 1))
  return a


print(sum(0))