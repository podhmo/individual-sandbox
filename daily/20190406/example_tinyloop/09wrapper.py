from functools import wraps


def maybe(itr):
    d = next(itr)
    if d is None:
        return None
    try:
        while True:
            d = itr.send(d)
            if d is None:
                return None
    except StopIteration as e:
        return e.args[0]


class Wrapper:
    def __init__(self, *, flavor=None):
        self.flavor = flavor

    def __call__(self, fn):
        @wraps(fn)
        def wrapped(*args, **kwargs):
            return self.flavor(fn(*args, **kwargs))

        return wrapped


action = Wrapper(flavor=maybe)


@action
def access(d, ks):
    for k in ks:
        d = yield d.get(k)
    return d


d = {"x": {"y": {"z": 10}}}
print(access(d, ["x", "y"]))


@action
def main(ks0, ks1):
    d = {"x": {"y": {"z": 10}}}
    x = yield access(d, ks0)
    y = yield access(d, ks1)
    return [x, y]


print(">", main(["x", "y"], ["x", "y"]))
print(">", main(["x", "y"], ["x", "z"]))
