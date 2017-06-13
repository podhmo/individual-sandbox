import warnings


def foo():
    warnings.warn("foo", stacklevel=2)
    return "hai"


if __name__ == "__main__":
    print(foo())
