def wrap(itr):
    v = next(itr)
    print("<<<", v)
    try:
        while True:
            print(">>>", [v])
            v = itr.send([v])
            print("<<<", v)
    except StopIteration as e:
        return e.args[0]


def gen():
    x = yield 1
    y = yield 2
    return x, y


print(wrap(gen()))
