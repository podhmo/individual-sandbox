def generate(L):
    for i, x in L:
        yield i, x, x * x


def consume(itr):
    for xs in itr:
        print("@", xs)


if __name__ == "__main__":
    L = enumerate([1, 2, 3, 4, 5])
    itr = generate(L)
    print("id, x, x * x")
    print("----------------------------------------")
    consume(itr)
