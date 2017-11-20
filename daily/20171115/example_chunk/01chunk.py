import itertools


def chunk(iterable, n):
    it = iter(iterable)
    while True:
        chunk_it = itertools.islice(it, n)
        try:
            first_el = next(chunk_it)
        except StopIteration:
            return
        yield tuple(itertools.chain((first_el, ), chunk_it))


L = list(range(31))
for i, x in enumerate(chunk(L, 5)):
    print(i, x)
