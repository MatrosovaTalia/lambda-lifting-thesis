from tkinter import N


def main():
    n = 1
    def eval_n():
        if n < 1:
            def plus_1():
                n = n + 1
                return n

    n = eval_n()
    return n