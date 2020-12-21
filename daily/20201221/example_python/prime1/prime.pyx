from libc.math cimport sqrt


def is_prime(X: int) -> bool:
    cdef long i, stop
    cdef long x = X

    stop = long(sqrt(x) + 1)
    if x <= 1:
        return False
    for i in range(2, stop):
        if x % i == 0:
            return False
    return True
