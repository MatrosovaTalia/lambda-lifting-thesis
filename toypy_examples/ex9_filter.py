def filter(condition, sequence):
    result = []
    for i in sequence:
        if condition(i):
            result.append(i)
        #end
    return result
    # end
#end

def is_positive(x):
    if x > 0:
        return True
    else:
        return False
    #end
#end
    

print(filter(is_positive, [-1, -18, 0, 0, 2, 3]))