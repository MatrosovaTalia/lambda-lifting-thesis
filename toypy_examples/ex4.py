# Before lambda lifting
# def f(a, b):
#     def g():
#         def k():
#             nums = [0, 1, 2, 3, 4, 5, 6, 777, 8, 9]
#             if nums[b % 10] == 777:
#                 return True
#             else:
#                 return False
#         if k():
#             return b * b
#         else:
#             return a * b + 1
#     def t():
#         return a * a + 7

#     if g() > 100:
#         return 1 + t()
#     else:
#         return t()



 # Step I: replace free variables in each function with a parameter:
 # in k: b -> n
 # in g: a -> x; b -> y
 # in t: a -> x
# def f(a, b):
#     def g(x, y):
#         def k(n):
#             nums = [0, 1, 2, 3, 4, 5, 6, 777, 8, 9]
#             if nums[n % 10] == 777:
#                 return True
#             else:
#                 return False
#         if k(b):
#             return y * y
#         else:
#             return x * y + 1
#     def t(x):
#         return x * x + 7

#     if g(a, b) > 100:
#         return 1 + t(a)
#     else:
#         return t(a)


# # Step II: lift k to a global scope
# def k(n):
#             nums = [0, 1, 2, 3, 4, 5, 6, 777, 8, 9]
#             if nums[n % 10] == 777:
#                 return True
#             else:
#                 return False

# def f(a, b):
#     def g(x, y):
#         if k(y):
#             return y * y
#         else:
#             return x * y + 1
#     def t(x):
#         return x * x + 7

#     if g(a, b) > 100:
#         return 1 + t(a)
#     else:
#         return t(a)

# Step III: lift g and t to a global scope
def k(n):
    nums = [0, 1, 2, 3, 4, 5, 6, 777, 8, 9]
    if nums[n % 10] == 777:
        return True
    else:
        return False

def g(x, y):
    if k(y):
        return y * y
    else:
        return x * y + 1

def t(x):
    return x * x + 7


def f(a, b):
    if g(a, b) > 100:
        return 1 + t(a)
    else:
        return t(a)


print(f(3, 4))