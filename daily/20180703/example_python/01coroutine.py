def consume(itr):
    for i, x in itr:
        print("@", i, x, x * x)


def coro():
    i = 0
    while True:
        val = yield None
        consume([[i, val]])
        i += 1


c = coro()
next(c)
c.send(10)
c.send(20)
c.send(30)
