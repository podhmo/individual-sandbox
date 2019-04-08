def maybe(g):
    d = yield next(g)
    if d is None:
        return None
    try:
        while True:
            d = yield g.send(d)
            if d is None:
                return None
    except StopIteration as e:
        return e.args[0]


def logged(g):
    d = yield next(g)
    print("<<<", d)
    try:
        while True:
            d = yield g.send(d)
            print("<<<", d)
    except StopIteration as e:
        try:
            return e.args[0]
        except IndexError:
            return None


def run(mg):
    v = next(mg)
    try:
        while True:
            v = mg.send(v)
    except StopIteration as e:
        try:
            return e.args[0]
        except IndexError:
            return None


def access(d, ks):
    for k in ks:
        d = yield d.get(k)
    return d


d = {"x": {"y": {"z": 10}}}
print(run(access(d, ["x", "y", "z"])))
print("----------------------------------------")
print(run(maybe(access(d, ["x", "y", "z"]))))
print("----------------------------------------")
print(run(logged(maybe(access(d, ["x", "y", "z"])))))

print("----------------------------------------")
print(run(maybe(access(d, ["x", "z"]))))
print("----------------------------------------")
print(run(logged(maybe(access(d, ["x", "z"])))))
