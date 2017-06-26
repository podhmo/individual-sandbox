from kamidana import as_filter


@as_filter
def emphasis(x):
    return " ".join(list(x))
