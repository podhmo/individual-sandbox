def g():
    yield from [1, 2, 3]


def f():
    print("start")
    for i in g():
        print(i)
    print("end")


f()
