# This one is the example from the article.
# I cannot understand how to adapt it to python-like language. 

# (define (bar x y)
#     (let ((f (lambda(u)(lambda(v)(+ u v)))))
# "    - f is applied to lambda(u) and then result is applied to (lambda(v)(+ u v))?"
#     ((f x) y) ))


def bar(x, y):
    def lambda_1(u):
        def lambda_2(v):
            return u + v
        return lambda_2
    f = lambda_1
    return f(x)(y)


# (define  plus
#     (lambda (x)
#         (lambda (y) (+ y x )) ))

def lambda_3(x):
    def lambda_4(y):
        return y + x
    return lambda_4
plus = lambda_3
