from prime import is_prime  # Cython を利用して定義した is_prime() を import


def minimum_prime_number():
    X = int(input().strip())
    answer = 0
    for i in range(X, (10 ** 14 + 32)):
        if is_prime(i):
            answer = i
            break
    print(answer)


if __name__ == '__main__':
    minimum_prime_number()
