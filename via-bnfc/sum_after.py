def sum_2(x, y):
    return x + y

def sum(n):
    if n == 1:
        return 1
    else:
        return sum_2(n, sum(n - 1))
