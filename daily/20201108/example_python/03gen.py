def gen():
    try:
        yield 1
        yield 2
        yield 3
        yield 4  # never
        yield 5  # never
    except GeneratorExit as e:
        print("!?", e)


def use(gen):
    def _use(n):
        print(n + 1)

    for i in gen:
        _use(i)
        if i == 3:
            break


use(gen())
