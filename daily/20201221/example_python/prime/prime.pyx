# https://qiita.com/zettaittenani/items/3bb25752d6e6a458332e

import cython
import math


def is_prime(x: int) -> bool:
    cdef:
        long i, stop  # C 言語の型を指定して変数宣言する
    stop = math.floor(math.sqrt(x) + 1)
    if x <= 1:
        return False
    for i in range(2, stop):
        if x % i == 0:
            return False
    return True
