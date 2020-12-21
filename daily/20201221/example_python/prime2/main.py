from prime import is_prime  # C++ で定義した関数 is_prime() を import


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
