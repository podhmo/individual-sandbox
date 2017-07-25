import sys


def gen():
    print("hai")
    yield 1
    print("hoi")


def f():
    try:
        it = gen()
        print(next(it))
        1 / 0
    except:
        t, v, tb = sys.exc_info()
        it.throw(t, v, tb)


def g():
    it = gen()
    print(next(it))
    1 / 0


# f()
g()
