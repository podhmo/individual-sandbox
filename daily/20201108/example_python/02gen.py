def gen():
    yield 1
    yield 2
    try:
        yield None
    except TypeError as e:
        print("hmm", e)
        yield 100
    yield 5  # ok


def use(gen):
    def _use(n):
        print(n + 1)

    for i in gen:
        try:
            _use(i)
        except TypeError as e:
            print("!", gen.throw(e))


use(gen())
