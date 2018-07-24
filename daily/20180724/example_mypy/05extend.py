import typing as t
import typing_extensions as tx
import mypy_extensions as mx


class HasA(tx.Protocol):
    a: str


class HasB(tx.Protocol):
    b: str


class AB(HasA, HasB, tx.Protocol):
    pass


def use(ob: AB) -> None:
    print(ob.a, ob.b)


T = t.TypeVar("T")


class EAB(t.Generic[T]):
    a: str
    b: str

    class Params(mx.TypedDict):
        a: str
        b: str


def extend(ob: T, d: EAB.Params) -> EAB[T]:
    x = t.cast(t.Any, ob)
    x.a = d["a"]
    x.b = d["b"]
    return t.cast(EAB[T], x)


def main() -> None:
    class Ob:
        pass

    ob = Ob()
    use(extend(ob, {"a": "foo", "b": "bar"}))


if __name__ == "__main__":
    main()
