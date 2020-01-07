import typing as t


def annotated(origin: t.Type[t.Any], *params: t.Any) -> t.Type[t.Any]:
    if not hasattr(origin, "__metadata__"):
        origin.__metadata__ = {}
    for x in params:
        origin.__metadata__[x.__name__] = x
    return origin


class A:
    pass


class B:
    pass


A = annotated(A, B) # type: ignore
print(A.__name__)
print(A.__metadata__)
