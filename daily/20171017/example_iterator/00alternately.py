from itertools import cycle, islice, chain


def rep(xs, n):
    for i in range(n):
        yield i, xs


def alternately(*xs):
    itrs = [iter(x) for x in xs]
    while itrs:
        poped = []
        for itr in itrs:
            try:
                yield next(itr)
            except StopIteration:
                poped.append(itr)
        if poped:
            itrs = [itr for itr in itrs if itr not in poped]


def roundrobin(*iterables):
    "roundrobin('ABC', 'D', 'EF') --> A D E B F C"
    # Recipe credited to George Sakkis
    pending = len(iterables)
    nexts = cycle(iter(it).__next__ for it in iterables)
    while pending:
        try:
            for next in nexts:
                yield next()
        except StopIteration:
            pending -= 1
            nexts = cycle(islice(nexts, pending))


xs = list(range(5))
itr0 = rep(xs, 3)
itr1 = rep(xs, 3)

for x in alternately(itr0, itr1):
    print(x)

xs = list(range(5))
itr0 = rep(xs, 3)
itr1 = rep(xs, 3)

for x in chain.from_iterable(zip(itr0, itr1)):
    print(x)

xs = list(range(5))
itr0 = rep(xs, 3)
itr1 = rep(xs, 3)

for x in roundrobin(itr0, itr1):
    print(x)
