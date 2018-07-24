import typing as t
import typing_extensions as tx

T = t.TypeVar("T")


class X(tx.Protocol):
    x: str


class TX(X, t.Generic[T]):
    pass


def extend_x(ob: T) -> TX[T]:
    ob2 = t.cast(t.Any, ob)
    ob2.x = "foo"
    return t.cast(TX[T], ob2)


class Y(tx.Protocol):
    y: str


class TY(Y, t.Generic[T]):
    pass


def extend_y(ob: T) -> TY[T]:
    ob2 = t.cast(t.Any, ob)
    ob2.y = "foo"
    return t.cast(TY[T], ob2)


def main() -> None:
    class Ob:
        pass

    ob = Ob()
    ob2 = extend_x(ob)
    print(ob2.x)

    ob3 = extend_y(ob2)
    print(ob3.y)
    print(ob3.x)  # error


#     class EFull(EY[Ob], EX[Ob]):
#         pass

#     ob4 = t.cast(EFull, ob3)
#     print(ob4.y)
#     print(ob4.x)

# if __name__ == "__main__":
#     main()
