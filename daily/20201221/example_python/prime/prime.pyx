# https://qiita.com/zettaittenani/items/3bb25752d6e6a458332e

import cython
from libc.math cimport sqrt


def is_prime(x: long) -> bool:
    cdef:
        long i, stop  # C 言語の型を指定して変数宣言する
    stop = long(sqrt(x) + 1)
    if x <= 1:
        return False
    for i in range(2, stop):
        if x % i == 0:
            return False
    return True
