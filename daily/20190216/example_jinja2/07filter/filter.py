from kamidana import as_filter


@as_filter
def xxx(x):
    return yyy(x)


def yyy(x):
    raise RuntimeError("oops")
