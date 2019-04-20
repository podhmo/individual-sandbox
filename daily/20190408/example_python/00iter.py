import itertools


def consume(itr):
    for line in itr:
        yield line
        if line == 2:
            yield from consume(itertools.chain([-2], itr))


print(list(consume(iter(range(5)))))
