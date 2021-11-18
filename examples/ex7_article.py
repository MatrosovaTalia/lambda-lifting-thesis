# This one is the example from the article.

# Paper: 

# (define (bar x y)
#     (let ((f (lambda(u)(lambda(v)(+ u v)))))
# "    - f is applied to lambda(u) and then result is applied to (lambda(v)(+ u v))?"
#     ((f x) y) ))

# Lambda lifting is not possible here: F contains non-liftable lambda term.

def bar(x, y):
    def f(a):
        def g1(u):
            def g2(v):
                return u + v
            return u
    return f(x)()




def plus():
    def x():
        return x
    def y():
        return y + x