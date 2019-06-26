def use():
    while True:
        v = yield
        print("<", v)

g = use()
next(g)
g.send(10)
g.send(20)
# -- stdout --------------------
# >> < 10
# >> < 20
