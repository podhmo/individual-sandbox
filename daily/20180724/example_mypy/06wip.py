import typing as t

T = t.TypeVar("T")


class EX(t.Generic[T]):
    x: str


def extend_x(ob: T) -> EX[T]:
    ob2 = t.cast(t.Any, ob)
    ob2.x = "foo"
    return t.cast(EX[T], ob2)


class EY(t.Generic[T]):
    y: str


def extend_y(ob: T) -> EY[T]:
    ob2 = t.cast(t.Any, ob)
    ob2.y = "foo"
    return t.cast(EY[T], ob2)


def main() -> None:
    class Ob:
        pass

    ob = Ob()
    ob2 = extend_x(ob)
    print(ob2.x)

    ob3 = extend_y(ob2)
    print(ob3.y)
    print(ob3.x)  # error

    class EFull(EY[Ob], EX[Ob]):
        pass

    ob4 = t.cast(EFull, ob3)
    print(ob4.y)
    print(ob4.x)


if __name__ == "__main__":
    main()
