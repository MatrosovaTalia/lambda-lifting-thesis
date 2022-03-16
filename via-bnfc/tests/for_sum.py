def main():
    a = 2
    e = 0
    for i in range(0, 5):
        b = a + i 
        def mul():
            c = a * b
            return c
        e = mul()
    
    return e

