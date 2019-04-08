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

run(access(d, ["x", "y", "z"]))  # => 10
run(maybe(access(d, ["x", "y", "y"])))  # => None
run(logged(maybe(access(d, ["x", "y", "y"]))))  # => None
run(logged(maybe(access(d, ["x", "y", "z"]))))  # => 10

# -- stdout --------------------
# <<< {'y': {'z': 10}}
# <<< {'z': 10}
# <<< None
# <<< {'y': {'z': 10}}
# <<< {'z': 10}
# <<< 10
