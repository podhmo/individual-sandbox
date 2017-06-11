import warnings


def useless(msg, cls=UserWarning):
    def _useless(fn):
        def decorated(*args, **kwargs):
            warnings.warn(msg, stacklevel=2)
            return fn(*args, **kwargs)

        return decorated

    return _useless


@useless("foo is useless")
def foo():
    return "foo"


if __name__ == "__main__":
    print(foo())
