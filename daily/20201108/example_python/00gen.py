def gen():
    yield 1
    yield 2
    yield 3


def use(gen):
    def _use(n):
        print(n)

    for i in gen:
        _use(i)


use(gen())
