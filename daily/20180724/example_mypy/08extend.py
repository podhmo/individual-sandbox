import typing as t
import typing_extensions as tx
import mypy_extensions as mx


class HasA(tx.Protocol):
    a: str


class HasB(tx.Protocol):
    b: str


T = t.TypeVar("T")


class AB(HasA, HasB, t.Generic[T]):
    pass


def use(ob: AB) -> None:
    print(ob.a, ob.b)


class Params(mx.TypedDict):
    a: str
    b: str


def extend(ob: T, d: Params) -> AB[T]:
    x = t.cast(t.Any, ob)
    x.a = d["a"]
    x.b = d["b"]
    return t.cast(AB[T], x)


def main() -> None:
    class Ob:
        z = "boo"

    ob = Ob()
    ob2 = extend(ob, {"a": "foo", "b": "bar"})
    use(ob2)
    print(ob.z)
    print(ob2.z)


if __name__ == "__main__":
    main()
