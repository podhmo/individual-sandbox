import math
from numba import jit


@jit(cache=True, nopython=True, fastmath=True)
def is_prime(x: int) -> bool:
    stop = int(math.sqrt(x) + 1)
    if x <= 1:
        return False
    for i in range(2, stop):
        if x % i == 0:
            return False
    return True


def minimum_prime_number(X: int):
    answer = 0
    for i in range(X, (10 ** 14 + 32)):
        if is_prime(i):
            answer = i
            break
    print(answer)


if __name__ == "__main__":
    import sys

    X = int(sys.argv[1].strip())
    minimum_prime_number(X)
