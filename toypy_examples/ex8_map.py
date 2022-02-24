def map(fun, sequence):
    result = []
    for i in sequence:
        result.append(fun(i))
        #end
    return result
    # end
    

# I am not sure about implementing for each loop.
def map_l(fun, n, sequence):
    result = []
    for i in range(0, n):
        result.append(fun(sequence[i]))
        #end
    return result
    # end

def add_1(x):
    return x + 1
    #end

print(map(add_1, 3, [1, 2, 3]))