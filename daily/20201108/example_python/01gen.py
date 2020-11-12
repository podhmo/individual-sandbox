def gen():
    yield 1
    yield 2
    try:
        yield None
    except Exception as e:
        print("hmm", e)
    yield 4  # never


def use(gen):
    def _use(n):
        print(n + 1)

    for i in gen:
        try:
            _use(i)
        except TypeError as e:
            gen.throw(e)


use(gen())
