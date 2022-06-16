def sum(n):
  def sum2(x):
    return (x + n)
  return sum2(sum(n - 1))