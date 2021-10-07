# Before lambda lifting:
def f(x, y):
    def k(z):
        return x + z
    return g(k)

def g(k):
    return k(1)

# Step I: convert free variable z to a parameter a. 
def f(x, y):
    def k(a, z):
        return a + z
    return g(k, x)

def g(k, a):
    return k(1, a)

# Step II: lift k to the global scope.
def k(a, z):
        return a + z

def f(x, y):
    return g(k, x)

def g(k, a):
    return k(1, a)


print(f(4, 0))