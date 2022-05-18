def sum(x, y):
    def sum_2(z):
        s = 0
        for i in range(x, z + 1):
            s = s + 1
    def bad(x):
        sum_2(0)
    return bad(3)