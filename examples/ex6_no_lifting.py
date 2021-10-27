# Lambda lifting is not possible in this example.
# I have found this example on the internet. 
# I do not understand why lambda lifting does not work here.

# def add_x(n):
#     x = 1
#     def sum_two_nums(y):
#         return x + y
#     return sum_two_nums

# # Trying step I:
# def add_x(n):
#     x = 1
#     def sum_two_nums(a, y):
#         return a + y
#     return sum_two_nums

# Step II:
def sum_two_nums(a, y):
        return a + y
def add_x(n):
    x = 1
    return sum_two_nums
a = add_x(6)
print(a)


