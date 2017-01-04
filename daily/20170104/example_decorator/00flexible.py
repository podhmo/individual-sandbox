from functools import partial, wraps


def flexible(decorator):
    @wraps(decorator)
    def _deco(f=None, **kwargs):
        def wrapped(f, *args, **kwargs):
            return f(*args, **kwargs)
        if f is None:
            return partial(_deco, **kwargs)
        else:
            return partial(wrapped, f, **kwargs)
    return _deco


def log(f, y=1):
    @wraps(f)
    def wrap(x):
        return f(x, y=y)
    return wrap


@flexible(log)
def inc(x, y=1):
    return x + y


f = flexible(log)


@f(y=20)
def inc2(x, y=1):
    return x + y

print(inc(10))
print(inc2(10))
