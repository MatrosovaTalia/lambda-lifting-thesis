# # Before lambda lifting
def sum(n):
    if n == 1:
        return 1
    else:
        def sum_2(x):
            return (x + n)
        return sum_2(sum(n - 1))
    

# # Step 1: convert free variable to a parameter.
# def sum(n):
#     if n == 1:
#         return 1
#     else:
#         def sum_2(x, y):
#             return (x + y)
#         return sum_2(n, sum(n - 1))
#     end
        
# # Step II: Lift sum_2 to the global scope.
def sum_2(x, y):
    return x + y
    end
def sum(n):
    if n == 1:
        return 1
    else:
        return sum_2(n, sum(n - 1))
    end


# print(sum(5))






# def raise_power(x, exp):
#    return power(x, exp)

# def power(base, exp):
#     return base ** exps

# def raise_power(exp):
#     def power(base):
#         return base ** exp
#     return power

# raise_2 = raise_power(2)

# print(raise_2(3))