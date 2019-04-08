def f():
    x = yield 1
    print("@", x)
    y = yield 2
    print("@", y)
    return x, y


itr = f()
v = next(itr)
print("!", v)
v = itr.send([v])
print("!", v)
try:
    print(itr.send([v]))
except StopIteration as e:
    print(e.args)
