import itertools


def chunk(xs, n):
    it = iter(iterable)
    while True:
        chunk_it = itertools.islice(it, n)
        try:
            first_el = next(chunk_it)
        except StopIteration:
            return
        yield itertools.chain((first_el,), chunk_it)


if __name__ == "__main__":
    print("hoi")
    xs = range(1, 11)
    print(list(map(list, chunk(xs, 4))))
    xs = range(1, 1 + 3)
    print(list(map(list, chunk(xs, 4))))
    xs = range(1, 1 + 4)
    print(list(map(list, chunk(xs, 4))))
    xs = range(1, 1 + 5)
    print(list(map(list, chunk(xs, 4))))
    xs = range(1, 1 + 8)
    print(list(map(list, chunk(xs, 4))))
