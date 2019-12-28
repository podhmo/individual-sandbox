from functools import wraps


def foo(fn):
    @wraps(fn)
    def _foo(*args, **kwargs):
        print("<<<foo")
        v = fn(*args, **kwargs)
        print(">>>foo")
        return v

    return _foo


def bar(fn):
    @wraps(fn)
    def _bar(*args, **kwargs):
        print("<<<bar")
        v = fn(*args, **kwargs)
        print(">>>bar")
        return v

    return _bar


@bar
@foo
def hello():
    """**hello world**"""
    return "hello world"


print(hello)
print(hello())
print(hello.__doc__)
