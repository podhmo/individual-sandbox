from functools import wraps


def access(d, ks):
    for k in ks:
        d = yield d[k]
    return d


def maybe(itr):
    d = next(itr)
    try:
        while True:
            d = itr.send(d)
    except KeyError:
        return None
    except StopIteration as e:
        return e.args[0]


def either(itr):
    d = next(itr)
    try:
        while True:
            d = itr.send(d)
    except KeyError as e:
        return None, e
    except StopIteration as e:
        return e.args[0], None


d = {"x": {"y": {"z": 10}}}
maybe(access(d, ["x", "y"]))  # => {'z': 10}
maybe(access(d, ["x", "z"]))  # => None

either(access(d, ["x", "y"]))  # => ({'z': 10}, None)
either(access(d, ["x", "z"]))  # => (None, KeyError('z'))


def logged(fn):
    import time

    @wraps(fn)
    def _logged(*args, **kwargs):
        print(f"start:{fn.__name__}	{time.monotonic()}")
        itr = fn(*args, **kwargs)
        v = next(itr)
        i = 0
        print(f"cont{i}:{fn.__name__}	{time.monotonic()}	{v}")
        try:
            while True:
                i += 1
                v = itr.send(v)
                print(f"cont{i}:{fn.__name__}	{time.monotonic()}	{v}")
        except StopIteration as e:
            print(f"  end:{fn.__name__}	{time.monotonic()}")
            return e.args[0]

    return _logged


@logged
def do_task(x, y):
    x = yield x
    y = yield y
    return x + y


do_task(10, 20)  # => 30
# -- stdout --------------------
# >> start:do_task	181380.097888821
# >> cont0:do_task	181380.09789752	10
# >> cont1:do_task	181380.097900104	20
# >>   end:do_task	181380.097902266
