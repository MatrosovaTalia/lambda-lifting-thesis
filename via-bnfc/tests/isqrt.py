def isqrt(n):
    if n < 0:
        return -1
    else:
        def isqrt_helper(s):
            if s * s <= n and n < (s + 1) * (s + 1):
                return s
            else:
                return isqrt_helper((s + (n / s)) / 2)
        return isqrt_helper((n + 1) / 2)