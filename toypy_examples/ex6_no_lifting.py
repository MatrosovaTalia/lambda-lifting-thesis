# Lambda lifting is not possible in this example.

# sum_two_nums is non-liftable, because it occurs in a non-function position:
# As a return value.

def add_x(n):
    x = 1
    x = x + 4
    def sum_two_nums(y):

        return x + y
    return sum_two_nums




