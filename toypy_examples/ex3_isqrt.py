# Before lambda lifting:
# def isqrt(n):
#     if n < 0:
#         return -1
#     else:
#         def isqrt_helper(s):
#             if s * s <= n and n < (s + 1) * (s + 1):
#                 return s
#             else:
#                 return isqrt_helper((s + (n / s)) / 2)
#         return isqrt_helper((n + 1) / 2)

# print(isqrt(32))

# Step I: replace free variale n to a parameter x:
# def isqrt(n):
#     if n < 0:
#         return -1
#     else:
#         def isqrt_helper(s, x):
#             if s * s <= x and x < (s + 1) * (s + 1):
#                 return s
#             else:
#                 return isqrt_helper((s + (x / s)) / 2, x)
#         return isqrt_helper((n + 1) / 2, n)

# print(isqrt(25))

# Step II: Lift isqrt_leper to a global scope:
def isqrt_helper(s, x):
            if s * s <= x and x < (s + 1) * (s + 1):
                return s
            else:
                return isqrt_helper((s + (x / s)) / 2, x)


def isqrt(n):
    if n < 0:
        return -1
    else:
        return isqrt_helper((n + 1) / 2, n)

print(isqrt(25))


