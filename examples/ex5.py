# def is_liar(a: bool, b: bool, n: int):
#     def alice(x: bool, y: bool):
#         if n > 20:
#             return x and y
#         else:
#             return True
#     if n < 5:
#         return not alice(a, b) 
#     else:
#         def bob(m: int):
#             if a:
#                 return m > 20 or not b
#             else:
#                 return a or b
#         return bob(n)

# Step I:
# def is_liar(a: bool, b: bool, n: int):
#     def alice(x: bool, y: bool, m: int):
#         if m > 20:
#             return x and y
#         else:
#             return True
#     if n < 5:
#         return not alice(a, b, n) 
#     else:
#         def bob(x: bool, y: bool, m: int):
#             if x:
#                 return m > 20 or not y
#             else:
#                 return x or y
#         return bob(a, b, n)

# # Step II:
def alice(x: bool, y: bool, m: int):
        if m > 20:
            return x and y
        else:
            return True

def bob(x: bool, y: bool, m: int):
            if x:
                return m > 20 or not y
            else:
                return x or y

def is_liar(a: bool, b: bool, n: int):
    if n < 5:
        return not alice(a, b, n) 
    else:
        return bob(a, b, n)

print(is_liar(True, True, 3))