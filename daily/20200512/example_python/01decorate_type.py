import typing as t


class X:
    pass


class Y:
    pass


alias_map = {}

TypeT = t.TypeVar("TypeT", bound=type)


def alias(name: str, typ: t.Type[TypeT]) -> t.Type[TypeT]:
    alias_map[typ] = name
    return typ


def use(typ: t.Type[X]) -> None:
    pass


XorY = alias("XorY", t.Union[X, Y])
reveal_type(XorY)
use(XorY)
_X = alias("_X", X)
reveal_type(_X)
use(_X)
