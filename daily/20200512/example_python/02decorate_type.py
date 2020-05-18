import typing as t

TypeT = t.TypeVar("TypeT", bound=type)


class Deco:
    mapping: t.Dict[t.Type[t.Any], t.Any]

    @classmethod
    def __class_getitem__(cls, typ: TypeT) -> TypeT:
        return typ


class X:
    pass


class Y:
    pass


def use(x: t.Type[X]) -> None:
    pass


DX = Deco[X]
print(use(X))
print(use(DX))
