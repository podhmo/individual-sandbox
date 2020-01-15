def gen():
    print((yield None))
    print((yield 1))
    print((yield 2))


g = gen()
next(g)
print(g.send(10))
print(g.send(20))

