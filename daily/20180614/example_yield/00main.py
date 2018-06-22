def f():
    x = yield 1
    y = (yield 1) + 2
    z = 2 + yield 1
f()
